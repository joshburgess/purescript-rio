"use strict";

// Internal interpreter for the rio-fiber prototype.
//
// `Op` is a tagged-object instruction tree built by the FFI factories
// below. The interpreter is structured as a `Fiber` object that owns
// the step loop, the continuation-frame stack, and the completion
// observer list. Synchronous programs complete in a single `step()`
// call; async programs suspend on an ASYNC op and resume when their
// registered callback fires.
//
// The architectural shape (tagged-object instructions + continuation
// stack + observer-completion) is the same well-known fiber pattern
// used by Effect / ZIO / Cats Effect.

// Op tags. Stored as small integers so the dispatch can be a single
// `switch` on a hot field.
const PURE = 0;
const SYNC = 1;
const BIND = 2;
const FAIL = 3;
const CATCH = 4;
const ASK = 5;
const LOCAL = 6;
const ASYNC = 7;
const FORK = 8;
const JOIN = 9;
const INTERRUPT = 10;
const ENSURING = 11;
const UNINTERRUPTIBLE = 12;
const RACE = 13;
const PAR_TRAVERSE = 14;
const PEEL = 15;
const FREF_GET = 16;
const FREF_SET = 17;
const FREF_MODIFY = 18;
const FAIL_CAUSE = 19;

// Continuation-stack frame tags.
const K_BIND = 0; // next: a -> Op r e b
const K_CATCH = 1; // handler: Variant e -> Op r e' a
const K_LOCAL = 2; // restore env: previous env to put back
const K_ENSURE = 3; // run finalizer regardless of outcome
const K_AFTER_FIN = 4; // restore saved (value, mode) after finalizer
const K_UNMASK = 5; // decrement mask depth
const K_PEEL = 6; // capture the current (mode, value) as a tagged result

// Fiber statuses.
const F_RUNNING = 0;
const F_SUSPENDED = 1;
const F_DONE = 2;

// Carrying modes. The (value, mode) pair encodes what's flowing
// through the continuation stack at any given moment.
const M_OK = 0;       // success: value carries an `a`
const M_FAIL = 1;     // typed failure: value carries a Variant e
const M_DIE = 2;      // defect: value carries a JS Error
const M_INTERRUPT = 3; // interrupt requested: value unused
// Composed cause: value carries a JS Cause object. Used when two
// independent failures need to be reported together (action + finalizer
// in K_AFTER_FIN; parallel branches in race/parTraverse if extended).
const M_CAUSE = 4;

// JS-side Cause tags. The PureScript Cause data type is reconstructed
// from these by walking the tree in `peelToCauseEither`.
const C_EMPTY = 0;
const C_FAIL = 1;
const C_DIE = 2;
const C_INTERRUPT = 3;
const C_THEN = 4;
const C_BOTH = 5;

const CAUSE_EMPTY = { _c: C_EMPTY };
const CAUSE_INTERRUPT = { _c: C_INTERRUPT };

function causeFail(v) { return { _c: C_FAIL, fail: v }; }
function causeDie(e) { return { _c: C_DIE, die: e }; }
function causeThen(a, b) {
  if (a._c === C_EMPTY) return b;
  if (b._c === C_EMPTY) return a;
  return { _c: C_THEN, left: a, right: b };
}
function causeBoth(a, b) {
  if (a._c === C_EMPTY) return b;
  if (b._c === C_EMPTY) return a;
  return { _c: C_BOTH, left: a, right: b };
}

function modeToCause(mode, value) {
  switch (mode) {
    case M_OK: return CAUSE_EMPTY;
    case M_FAIL: return causeFail(value);
    case M_DIE: return causeDie(value);
    case M_INTERRUPT: return CAUSE_INTERRUPT;
    case M_CAUSE: return value;
    default: return CAUSE_EMPTY;
  }
}

// Number of ops a fiber may execute before yielding to the microtask
// queue. Matches the Effect / ZIO default order of magnitude; tuned
// for V8's inlining of tight switch dispatches.
const TICK_BUDGET = 2048;

// Op factories ---------------------------------------------------------

export const opPure = function (a) {
  return { _tag: PURE, value: a };
};

export const opLiftEffect = function (eff) {
  return { _tag: SYNC, run: eff };
};

export const opBind = function (m) {
  return function (k) {
    return { _tag: BIND, op: m, next: k };
  };
};

// Singleton ASK: no payload, one shape, reuse the object.
const ASK_NODE = { _tag: ASK };
export const opAsk = ASK_NODE;

export const opFail = function (e) {
  return { _tag: FAIL, error: e };
};

export const opCatchAll = function (handler) {
  return function (op) {
    return { _tag: CATCH, op: op, handler: handler };
  };
};

export const opLocal = function (f) {
  return function (op) {
    return { _tag: LOCAL, transform: f, op: op };
  };
};

// `register :: (a -> Effect Unit) -> (Variant e -> Effect Unit) -> Effect (Effect Unit)`
// The fiber calls `register(onOk)(onFail)()` and gets back an
// `Effect Unit` canceller. `onOk` and `onFail` are curried PureScript
// functions; the fiber wraps the underlying single-shot resume.
export const opAsync = function (register) {
  return { _tag: ASYNC, register: register };
};

export const opFork = function (op) {
  return { _tag: FORK, op: op };
};

export const opJoin = function (fiber) {
  return { _tag: JOIN, fiber: fiber };
};

export const opInterrupt = function (fiber) {
  return { _tag: INTERRUPT, fiber: fiber };
};

export const opEnsuring = function (finalizer) {
  return function (action) {
    return { _tag: ENSURING, action: action, finalizer: finalizer };
  };
};

export const opUninterruptible = function (op) {
  return { _tag: UNINTERRUPTIBLE, op: op };
};

export const opRace = function (left) {
  return function (right) {
    return { _tag: RACE, left: left, right: right };
  };
};

export const opParTraverse = function (fn) {
  return function (items) {
    return { _tag: PAR_TRAVERSE, fn: fn, items: items };
  };
};

export const opPeel = function (op) {
  return { _tag: PEEL, op: op };
};

// FiberRef factories. Each FiberRef is a fresh JS object that doubles
// as a map key; identity is structural so two distinct refs created
// with the same initial value still address different slots.
export const _newFiberRef = function (initial) {
  return function () {
    return { initial: initial };
  };
};

export const opGetFiberRef = function (ref) {
  return { _tag: FREF_GET, ref: ref };
};

export const opSetFiberRef = function (ref) {
  return function (value) {
    return { _tag: FREF_SET, ref: ref, value: value };
  };
};

export const opModifyFiberRef = function (ref) {
  return function (fn) {
    return { _tag: FREF_MODIFY, ref: ref, fn: fn };
  };
};

// Scope: a JS object owning a list of synchronous `Effect Unit`
// finalizers. The MVP API is fire-and-forget: closeScope invokes
// each finalizer in LIFO order and swallows individual throws so
// one bad finalizer can't strand the rest.
function makeScope() {
  return { finalizers: [], closed: false };
}

export const _newScope = function () {
  return makeScope();
};

export const _addFinalizerEff = function (scope) {
  return function (fin) {
    return function () {
      if (scope.closed) {
        try { fin(); } catch (_) {}
        return;
      }
      scope.finalizers.push(fin);
    };
  };
};

export const _closeScope = function (scope) {
  return function () {
    if (scope.closed) return;
    scope.closed = true;
    const fs = scope.finalizers;
    scope.finalizers = [];
    for (let i = fs.length - 1; i >= 0; i--) {
      try { fs[i](); } catch (_) {}
    }
  };
};

// Fiber ----------------------------------------------------------------

// Global supervisor registry. Each entry is { onStart, onEnd } where
// each hook is a thunked Effect (i.e. a function returning a function
// returning Unit). Calls are wrapped in try/catch so a faulty
// supervisor can't crash the interpreter.
const _supervisors = [];
let _nextFiberId = 0;

export const _registerSupervisor = function (sup) {
  return function () {
    _supervisors.push(sup);
    return function () {
      const idx = _supervisors.indexOf(sup);
      if (idx >= 0) _supervisors.splice(idx, 1);
      return {};
    };
  };
};

function Fiber(op, env, frefs) {
  this.id = _nextFiberId++;
  this.current = op;
  this.value = null;
  this.mode = M_OK;
  this.stack = [];
  this.env = env;
  this.status = F_RUNNING;
  this.result = null;
  this.observers = [];
  this.interrupted = false;
  // Depth of nested uninterruptible regions. While > 0, interrupts
  // are deferred (the flag stays set; the step loop just doesn't
  // act on it).
  this.mask = 0;
  this.canceller = null;
  // Per-fiber state map keyed by FiberRef identity. Children copy
  // this on fork so each fiber gets its own isolated namespace.
  this.frefs = frefs || new Map();
  for (let i = 0; i < _supervisors.length; i++) {
    try {
      _supervisors[i].onStart(this.id)();
    } catch (_) {}
  }
}

Fiber.prototype._complete = function (result) {
  if (this.status === F_DONE) return;
  this.status = F_DONE;
  this.result = result;
  for (let i = 0; i < _supervisors.length; i++) {
    try {
      _supervisors[i].onEnd(this.id)();
    } catch (_) {}
  }
  const obs = this.observers;
  this.observers = [];
  for (let i = 0; i < obs.length; i++) {
    try {
      obs[i](result);
    } catch (_) {
      // observers must not throw into the fiber; swallow.
    }
  }
};

Fiber.prototype.observe = function (cb) {
  if (this.status === F_DONE) {
    cb(this.result);
  } else {
    this.observers.push(cb);
  }
};

Fiber.prototype.interrupt = function () {
  if (this.status === F_DONE) return;
  this.interrupted = true;
  if (this.status === F_SUSPENDED) {
    const c = this.canceller;
    this.canceller = null;
    if (c) {
      try {
        c();
      } catch (_) {
        // ignore canceller throws
      }
    }
    // Drive the fiber forward so it observes the interrupt flag at
    // its next safe point. We re-enter step() so any pending
    // finalizers run.
    this.status = F_RUNNING;
    this.step();
  }
  // If F_RUNNING the step loop will see the flag on its next pass.
};

// Install the result of a completed async / join into the fiber's
// (value, mode). Returns true if the fiber should continue stepping.
Fiber.prototype._installResult = function (r) {
  if (Object.prototype.hasOwnProperty.call(r, "die")) {
    this.value = r.die;
    this.mode = M_DIE;
    return true;
  }
  if (r.interrupted) {
    this.interrupted = true;
    return true;
  }
  if (Object.prototype.hasOwnProperty.call(r, "ok")) {
    this.value = r.ok;
    this.mode = M_OK;
    return true;
  }
  if (Object.prototype.hasOwnProperty.call(r, "cause")) {
    this.value = r.cause;
    this.mode = M_CAUSE;
    return true;
  }
  // fail
  this.value = r.fail;
  this.mode = M_FAIL;
  return true;
};

Fiber.prototype._completeFromMode = function () {
  switch (this.mode) {
    case M_OK:
      this._complete({ ok: this.value });
      return;
    case M_FAIL:
      this._complete({ fail: this.value });
      return;
    case M_DIE:
      this._complete({ die: this.value });
      return;
    case M_INTERRUPT:
      this._complete({ interrupted: true });
      return;
    case M_CAUSE:
      this._complete({ cause: this.value });
      return;
  }
};

Fiber.prototype.step = function () {
  let ticks = TICK_BUDGET;
  while (true) {
    if (--ticks < 0) {
      scheduleFiber(this);
      return;
    }

    // Consume a pending interrupt: transition into the unwinding
    // path so finalizers get a chance to run. Only consume when
    // we're not in a mask and not already in a failure unwind that
    // would override (interrupt has priority over typed failure but
    // not over a die we're already carrying). ENSURING and
    // UNINTERRUPTIBLE are structural and must be processed even
    // under interrupt so their frames get installed; the action
    // they wrap is then interruptible on the next iteration.
    if (
      this.interrupted &&
      this.mask === 0 &&
      this.mode !== M_INTERRUPT &&
      this.mode !== M_DIE &&
      this.mode !== M_CAUSE &&
      (this.current === null ||
        (this.current._tag !== ENSURING &&
          this.current._tag !== UNINTERRUPTIBLE))
    ) {
      this.current = null;
      this.mode = M_INTERRUPT;
      this.value = null;
    }

    if (this.current !== null) {
      const op = this.current;
      this.current = null;
      switch (op._tag) {
        case PURE:
          this.value = op.value;
          this.mode = M_OK;
          break;
        case SYNC:
          try {
            this.value = op.run();
            this.mode = M_OK;
          } catch (err) {
            // Synchronous throw becomes a defect that unwinds
            // through any pending finalizers.
            this.value = err;
            this.mode = M_DIE;
          }
          break;
        case BIND:
          this.stack.push({ _k: K_BIND, next: op.next });
          this.current = op.op;
          continue;
        case FAIL:
          this.value = op.error;
          this.mode = M_FAIL;
          break;
        case CATCH:
          this.stack.push({ _k: K_CATCH, handler: op.handler });
          this.current = op.op;
          continue;
        case ASK:
          this.value = this.env;
          this.mode = M_OK;
          break;
        case LOCAL: {
          const prev = this.env;
          this.env = op.transform(this.env);
          this.stack.push({ _k: K_LOCAL, prev: prev });
          this.current = op.op;
          continue;
        }
        case ASYNC: {
          const self = this;
          let settled = false;
          let syncResult = null;
          const onOk = function (a) {
            return function () {
              if (settled) return;
              settled = true;
              const r = { ok: a };
              if (self.status === F_RUNNING) {
                syncResult = r;
              } else {
                self._resumeAsync(r);
              }
            };
          };
          const onFail = function (v) {
            return function () {
              if (settled) return;
              settled = true;
              const r = { fail: v };
              if (self.status === F_RUNNING) {
                syncResult = r;
              } else {
                self._resumeAsync(r);
              }
            };
          };
          let canceller;
          try {
            canceller = op.register(onOk)(onFail)();
          } catch (err) {
            this.value = err;
            this.mode = M_DIE;
            break;
          }
          if (syncResult !== null) {
            this._installResult(syncResult);
            break;
          }
          this.status = F_SUSPENDED;
          this.canceller = canceller;
          return;
        }
        case FORK: {
          const child = new Fiber(op.op, this.env, new Map(this.frefs));
          scheduleFiber(child);
          this.value = child;
          this.mode = M_OK;
          break;
        }
        case JOIN: {
          const target = op.fiber;
          const self = this;
          if (target.status === F_DONE) {
            this._installResult(target.result);
            break;
          }
          this.status = F_SUSPENDED;
          this.canceller = null;
          target.observe(function (r) {
            self._resumeAsync(r);
          });
          return;
        }
        case INTERRUPT: {
          op.fiber.interrupt();
          this.value = undefined;
          this.mode = M_OK;
          break;
        }
        case ENSURING:
          this.stack.push({ _k: K_ENSURE, finalizer: op.finalizer });
          this.current = op.action;
          continue;
        case UNINTERRUPTIBLE:
          this.mask++;
          this.stack.push({ _k: K_UNMASK });
          this.current = op.op;
          continue;
        case RACE: {
          const self = this;
          const leftFiber = new Fiber(op.left, this.env, new Map(this.frefs));
          const rightFiber = new Fiber(op.right, this.env, new Map(this.frefs));
          let settled = false;
          let interruptedCount = 0;
          const onComplete = function (loser) {
            return function (r) {
              if (settled) return;
              if (r.interrupted) {
                interruptedCount++;
                if (interruptedCount === 2) {
                  settled = true;
                  self._resumeAsync(r);
                }
                return;
              }
              settled = true;
              loser.interrupt();
              self._resumeAsync(r);
            };
          };
          leftFiber.observe(onComplete(rightFiber));
          rightFiber.observe(onComplete(leftFiber));
          scheduleFiber(leftFiber);
          scheduleFiber(rightFiber);
          this.status = F_SUSPENDED;
          this.canceller = function () {
            leftFiber.interrupt();
            rightFiber.interrupt();
          };
          return;
        }
        case PEEL:
          this.stack.push({ _k: K_PEEL });
          this.current = op.op;
          continue;
        case PAR_TRAVERSE: {
          const self = this;
          const fn = op.fn;
          const items = op.items;
          const n = items.length;
          if (n === 0) {
            this.value = [];
            this.mode = M_OK;
            break;
          }
          const results = new Array(n);
          const fibers = new Array(n);
          let pending = n;
          let settled = false;
          for (let i = 0; i < n; i++) {
            const idx = i;
            const child = new Fiber(fn(items[idx]), this.env, new Map(this.frefs));
            fibers[idx] = child;
            child.observe(function (r) {
              if (settled) return;
              if (Object.prototype.hasOwnProperty.call(r, "ok")) {
                results[idx] = r.ok;
                pending--;
                if (pending === 0) {
                  settled = true;
                  self._resumeAsync({ ok: results });
                }
                return;
              }
              settled = true;
              for (let j = 0; j < n; j++) {
                if (j !== idx) fibers[j].interrupt();
              }
              self._resumeAsync(r);
            });
          }
          for (let i = 0; i < n; i++) {
            scheduleFiber(fibers[i]);
          }
          this.status = F_SUSPENDED;
          this.canceller = function () {
            for (let i = 0; i < n; i++) {
              fibers[i].interrupt();
            }
          };
          return;
        }
        case FAIL_CAUSE: {
          // Empty causes succeed (no failure to report); anything
          // else unwinds with M_CAUSE so the structured leaves are
          // visible at the top.
          if (op.cause._c === C_EMPTY) {
            this.value = undefined;
            this.mode = M_OK;
          } else {
            this.value = op.cause;
            this.mode = M_CAUSE;
          }
          break;
        }
        case FREF_GET: {
          const ref = op.ref;
          this.value = this.frefs.has(ref) ? this.frefs.get(ref) : ref.initial;
          this.mode = M_OK;
          break;
        }
        case FREF_SET: {
          this.frefs.set(op.ref, op.value);
          this.value = undefined;
          this.mode = M_OK;
          break;
        }
        case FREF_MODIFY: {
          const ref = op.ref;
          const prev = this.frefs.has(ref) ? this.frefs.get(ref) : ref.initial;
          let next;
          try {
            next = op.fn(prev);
          } catch (err) {
            this.value = err;
            this.mode = M_DIE;
            break;
          }
          this.frefs.set(ref, next);
          this.value = undefined;
          this.mode = M_OK;
          break;
        }
        default:
          this.value = new Error("rio-fiber: unknown Op tag " + op._tag);
          this.mode = M_DIE;
          break;
      }
    }

    // Unwinding loop. One pop per iteration. Each frame chooses
    // whether to consume the current (value, mode), pass it through,
    // or rewrite it.
    while (true) {
      if (
        this.interrupted &&
        this.mask === 0 &&
        this.mode !== M_INTERRUPT &&
        this.mode !== M_DIE &&
        this.mode !== M_CAUSE
      ) {
        this.mode = M_INTERRUPT;
        this.value = null;
      }
      if (this.stack.length === 0) {
        this._completeFromMode();
        return;
      }
      const frame = this.stack.pop();
      switch (frame._k) {
        case K_BIND:
          if (this.mode === M_OK) {
            this.current = frame.next(this.value);
          }
          // FAIL / DIE / INTERRUPT: skip this frame; keep unwinding.
          break;
        case K_CATCH:
          if (this.mode === M_FAIL) {
            this.current = frame.handler(this.value);
            this.mode = M_OK;
            this.value = null;
          }
          // OK passes through; DIE / INTERRUPT bypass user handlers.
          break;
        case K_LOCAL:
          this.env = frame.prev;
          break;
        case K_ENSURE: {
          // Save the action's outcome, enter uninterruptible region,
          // and arrange for the finalizer to run.
          this.mask++;
          this.stack.push({
            _k: K_AFTER_FIN,
            savedValue: this.value,
            savedMode: this.mode,
          });
          this.current = frame.finalizer;
          this.mode = M_OK;
          this.value = null;
          break;
        }
        case K_AFTER_FIN: {
          this.mask--;
          // Finalizer just completed. Compose its outcome with the
          // saved action outcome through `Cause.then_`:
          //   - both succeeded: drop back to the action's success value
          //   - either failed: propagate the composed Cause
          //   - both failed: both leaves are visible in the Cause
          if (this.mode === M_OK && frame.savedMode === M_OK) {
            this.value = frame.savedValue;
            this.mode = M_OK;
          } else if (this.mode === M_OK) {
            // finalizer ok; restore action's failure exactly
            this.value = frame.savedValue;
            this.mode = frame.savedMode;
          } else if (frame.savedMode === M_OK) {
            // action ok, finalizer failed; propagate finalizer
            // (mode/value already in place)
          } else {
            // both non-ok: compose
            const combined = causeThen(
              modeToCause(frame.savedMode, frame.savedValue),
              modeToCause(this.mode, this.value),
            );
            this.value = combined;
            this.mode = M_CAUSE;
          }
          break;
        }
        case K_UNMASK:
          this.mask--;
          break;
        case K_PEEL: {
          // Capture the current (mode, value) as a tagged result and
          // continue as a success. Clear the interrupt flag so the
          // captured outcome is the final word; if the caller wants
          // to re-propagate the interrupt they can do it from the
          // returned tagged result.
          let result;
          switch (this.mode) {
            case M_OK:
              result = { ok: this.value };
              break;
            case M_FAIL:
              result = { fail: this.value };
              break;
            case M_DIE:
              result = { die: this.value };
              break;
            case M_CAUSE:
              result = { cause: this.value };
              break;
            default:
              result = { interrupted: true };
              break;
          }
          this.value = result;
          this.mode = M_OK;
          this.interrupted = false;
          break;
        }
      }
      if (this.current !== null) break;
    }
  }
};

Fiber.prototype._resumeAsync = function (r) {
  // Late callbacks after interruption can arrive once the fiber has
  // already completed (e.g. a child observer for a RACE loser whose
  // result we no longer care about). Drop them silently.
  if (this.status === F_DONE) return;
  this.canceller = null;
  this.status = F_RUNNING;
  this._installResult(r);
  this.step();
};

// Microtask scheduler. queueMicrotask is available in modern Node and
// browsers; the setTimeout fallback covers ancient runtimes.
const scheduleFiber =
  typeof queueMicrotask !== "undefined"
    ? function (f) {
        queueMicrotask(function () {
          f.step();
        });
      }
    : function (f) {
        setTimeout(function () {
          f.step();
        }, 0);
      };

// Top-level entry points -----------------------------------------------

export const _startFiber = function (op) {
  return function (env) {
    return function () {
      const f = new Fiber(op, env);
      f.step();
      return f;
    };
  };
};

export const _fiberIsDone = function (f) {
  return f.status === F_DONE;
};

export const _fiberResult = function (f) {
  return f.result;
};

export const _fiberObserve = function (f) {
  return function (cb) {
    return function () {
      f.observe(function (r) {
        cb(r)();
      });
    };
  };
};

export const _fiberInterrupt = function (f) {
  return function () {
    f.interrupt();
  };
};

// Tagged-result inspectors used by the PureScript wrapper.
export const _resultIsOk = function (r) {
  return Object.prototype.hasOwnProperty.call(r, "ok");
};

export const _resultIsFail = function (r) {
  return Object.prototype.hasOwnProperty.call(r, "fail");
};

export const _resultIsInterrupted = function (r) {
  return r.interrupted === true;
};

export const _resultIsCause = function (r) {
  return Object.prototype.hasOwnProperty.call(r, "cause");
};

export const _resultOk = function (r) {
  return r.ok;
};

export const _resultFail = function (r) {
  return r.fail;
};

export const _resultDie = function (r) {
  return r.die;
};

export const _resultCause = function (r) {
  return r.cause;
};

// Cause-tree walkers. `_causeTag` returns the small integer matching
// C_EMPTY / C_FAIL / C_DIE / C_INTERRUPT / C_THEN / C_BOTH so the
// PureScript side can dispatch.
export const _causeTag = function (c) {
  return c._c;
};
export const _causeFailValue = function (c) {
  return c.fail;
};
export const _causeDieValue = function (c) {
  return c.die;
};
export const _causeLeft = function (c) {
  return c.left;
};
export const _causeRight = function (c) {
  return c.right;
};

// Smart constructors for the JS-side Cause tree. PureScript code uses
// these to build a Cause for `failCause`. The shapes are private to
// the interpreter; the PS side reconstructs a `Cause e` from them via
// the walker accessors above.
export const _causeEmpty = CAUSE_EMPTY;
export const _causeFail = function (v) { return causeFail(v); };
export const _causeDie = function (e) { return causeDie(e); };
export const _causeInterrupt = CAUSE_INTERRUPT;
export const _causeThen = function (a) {
  return function (b) { return causeThen(a, b); };
};
export const _causeBoth = function (a) {
  return function (b) { return causeBoth(a, b); };
};

export const opFailCause = function (c) {
  return { _tag: FAIL_CAUSE, cause: c };
};
