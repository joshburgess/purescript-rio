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
const FORK_INLINE = 20;
const FORK_ALL = 21;
const JOIN_ALL = 22;
const FOR_EACH = 23;

// Continuation-stack frame tags.
const K_BIND = 0; // next: a -> Op r e b
const K_CATCH = 1; // handler: Variant e -> Op r e' a
const K_LOCAL = 2; // restore env: previous env to put back
const K_ENSURE = 3; // run finalizer regardless of outcome
const K_AFTER_FIN = 4; // restore saved (value, mode) after finalizer
const K_UNMASK = 5; // decrement mask depth
const K_PEEL = 6; // capture the current (mode, value) as a tagged result
const K_FOR_EACH = 7; // sequential traverse: collect this iteration, advance

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

// Fiber-completion result objects are uniformly shaped as
// { mode, value } where `mode` is one of the M_* constants and
// `value` is the payload (or null for M_INTERRUPT). Reading the
// shape is one hidden-class read per dispatch instead of three
// hasOwnProperty checks.
function makeResult(mode, value) {
  return { mode: mode, value: value };
}

const RESULT_INTERRUPT = { mode: M_INTERRUPT, value: null };

// Lightweight stand-in for an already-completed Fiber. forkInline of
// a PURE / SYNC body produces a child whose result is known before we
// hand the handle to the parent; allocating a full Fiber object (14
// fields + supervisor loop + a step() round-trip) just to satisfy a
// later JOIN is wasteful. A `DoneFiber` carries the same fields JOIN
// and the FFI helpers read off a real `Fiber` (status / result /
// queued) plus mode / value inlined so the {mode, value} result
// object doesn't need a separate allocation: `this.result = this`
// makes `target.result.mode` and `target.result.value` resolve via
// the same hidden class.
function DoneFiber(mode, value) {
  this.queued = false;
  this.status = F_DONE;
  this.mode = mode;
  this.value = value;
  this.result = this;
}
DoneFiber.prototype.observe = function (cb) {
  // Match Fiber.observe's "already done" branch: deliver the result
  // immediately on the same call stack the joiner is on.
  cb(this);
};
DoneFiber.prototype.interrupt = function () {
  // Already complete; interruption is a no-op.
};

// Convert a fiber-completion result object into a Cause. Mirrors
// `modeToCause` for the result-object shape used by observers.
function resultToCause(r) {
  switch (r.mode) {
    case M_OK: return CAUSE_EMPTY;
    case M_FAIL: return causeFail(r.value);
    case M_DIE: return causeDie(r.value);
    case M_INTERRUPT: return CAUSE_INTERRUPT;
    case M_CAUSE: return r.value;
    default: return CAUSE_EMPTY;
  }
}

// Number of ops a fiber may execute before yielding to the microtask
// queue. Matches the Effect / ZIO default order of magnitude; tuned
// for V8's inlining of tight switch dispatches.
const TICK_BUDGET = 4096;

// Op factories ---------------------------------------------------------

export const opPure = function (a) {
  return { _tag: PURE, value: a };
};

export const opLiftEffect = function (eff) {
  return { _tag: SYNC, run: eff };
};

// The BIND op also doubles as its own K_BIND continuation frame
// (same `_k` field the unwind switch reads, same `next` field the
// frame would carry). When the step loop encounters a BIND it can
// push the op itself instead of allocating a fresh frame, saving
// one object allocation per bind on the hot path.
export const opBind = function (m) {
  return function (k) {
    return { _tag: BIND, _k: K_BIND, op: m, next: k };
  };
};

// Singleton ASK: no payload, one shape, reuse the object.
const ASK_NODE = { _tag: ASK };
export const opAsk = ASK_NODE;

// Singleton continuation frames that carry no per-instance payload:
// pushing the singleton avoids a per-step allocation. Frames are
// never mutated during the unwind, so sharing is safe.
const FRAME_UNMASK = { _k: K_UNMASK };
const FRAME_PEEL = { _k: K_PEEL };

export const opFail = function (e) {
  return { _tag: FAIL, error: e };
};

// CATCH op doubles as its own K_CATCH frame (same trick as opBind):
// the op carries `handler`, and the unwind switch only needs `_k`
// and `handler`. Pushing the op itself saves a per-step alloc.
export const opCatchAll = function (handler) {
  return function (op) {
    return { _tag: CATCH, _k: K_CATCH, op: op, handler: handler };
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

// Like opFork but steps the child synchronously before returning the
// handle to the parent. If the child's body is fully sync, the child
// completes inline and the parent's subsequent `join` resolves without
// touching the microtask scheduler. If the child suspends, the parent
// gets a live handle exactly as with opFork.
export const opForkInline = function (op) {
  return { _tag: FORK_INLINE, op: op };
};

export const opJoin = function (fiber) {
  return { _tag: JOIN, fiber: fiber };
};

// Specialized array fork: take N ops and produce N fiber handles in
// one step. Equivalent to `traverse fork ops` but skips the per-element
// BIND chain (which `traverseArrayImpl` builds as a balanced ~2N-node
// tree). The handler walks the array in a tight JS loop.
export const opForkAll = function (ops) {
  return { _tag: FORK_ALL, ops: ops };
};

// Specialized array join: take N fiber handles and produce their N
// results in order. Suspends until all complete; on the first non-OK
// outcome the parent propagates that outcome (sibling fibers continue
// running unmolested, matching ZIO's `Fiber.joinAll` semantics).
export const opJoinAll = function (fibers) {
  return { _tag: JOIN_ALL, fibers: fibers };
};

// Sequential traverse: run `fn(items[i])` for each i in order and
// collect the results. Skips the ~2N-node bind chain that
// `traverseArrayImpl` builds; instead the step loop holds a single
// K_FOR_EACH frame and advances `i` in place across resumptions.
export const opForEach = function (fn) {
  return function (items) {
    return { _tag: FOR_EACH, fn: fn, items: items };
  };
};

export const opInterrupt = function (fiber) {
  return { _tag: INTERRUPT, fiber: fiber };
};

// ENSURING op doubles as its own K_ENSURE frame: `finalizer` is on
// the op, the unwind switch only reads `_k` and `finalizer`.
export const opEnsuring = function (finalizer) {
  return function (action) {
    return { _tag: ENSURING, _k: K_ENSURE, action: action, finalizer: finalizer };
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
  // Lazy-allocate the observer list. Most short-lived fibers either
  // never gain an observer or gain exactly one (a single joiner) and
  // resolve before any second registration, so deferring the array
  // alloc until the first observer arrives avoids a per-fiber [].
  this.observers = null;
  this.interrupted = false;
  // Depth of nested uninterruptible regions. While > 0, interrupts
  // are deferred (the flag stays set; the step loop just doesn't
  // act on it).
  this.mask = 0;
  this.canceller = null;
  // True while this fiber is sitting in the run queue waiting for
  // its first step. JOIN uses this to drive the target synchronously
  // when the parent reaches the join before the microtask drains,
  // collapsing the per-fork microtask hop on the fan-in path. Cleared
  // by `_runDrain` (or by an inline-driving JOIN) before step().
  this.queued = false;
  // Per-fiber state map keyed by FiberRef identity. We stay lazy:
  // the Map itself is not allocated until either the fiber or one
  // of its ancestors actually writes a FiberRef. `frefs === null`
  // means "no writes seen yet, reads return ref.initial". Forks pass
  // through the parent's reference (possibly null); copy-on-write
  // on the next write by either side promotes both to owned Maps.
  // `frefsOwn = true` means we are the sole owner of `frefs` (or it
  // is null and any write should create a fresh Map).
  if (frefs !== undefined) {
    this.frefs = frefs;
    this.frefsOwn = false;
  } else {
    this.frefs = null;
    this.frefsOwn = true;
  }
  for (let i = 0; i < _supervisors.length; i++) {
    try {
      _supervisors[i].onStart(this.id)();
    } catch (_) {}
  }
}

Fiber.prototype._complete = function (result) {
  if (this.status === F_DONE) return;
  this.status = F_DONE;
  // Mirror DoneFiber's shape: store mode/value as direct fields and
  // self-ref `result`. JOIN and the FFI inspectors then read mode /
  // value off any "done thing" via the same hidden class, regardless
  // of whether it's a real Fiber or a DoneFiber stub.
  this.mode = result.mode;
  this.value = result.value;
  this.result = this;
  for (let i = 0; i < _supervisors.length; i++) {
    try {
      _supervisors[i].onEnd(this.id)();
    } catch (_) {}
  }
  // `observers` is null (no observer), a bare function (one observer),
  // or an array (two or more). The single-function shape skips the
  // per-observer Array allocation that the common one-joiner case
  // would otherwise pay.
  const obs = this.observers;
  this.observers = null;
  if (obs !== null) {
    if (typeof obs === "function") {
      try { obs(this); } catch (_) {}
    } else {
      for (let i = 0; i < obs.length; i++) {
        try { obs[i](this); } catch (_) {}
      }
    }
  }
};

Fiber.prototype.observe = function (cb) {
  if (this.status === F_DONE) {
    cb(this);
    return;
  }
  const obs = this.observers;
  if (obs === null) {
    // First observer: store the callback directly (no [cb] alloc).
    this.observers = cb;
  } else if (typeof obs === "function") {
    // Promote to array on the second observer.
    this.observers = [obs, cb];
  } else {
    obs.push(cb);
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
  const m = r.mode;
  if (m === M_INTERRUPT) {
    this.interrupted = true;
  } else {
    this.value = r.value;
    this.mode = m;
  }
  return true;
};

Fiber.prototype._completeFromMode = function () {
  if (this.mode === M_INTERRUPT) {
    this._complete(RESULT_INTERRUPT);
  } else {
    this._complete(makeResult(this.mode, this.value));
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
        case BIND: {
          // Fast paths for leaf inner ops. These cover the every-
          // bind patterns in `do { x <- pure y; ... }` /
          // `do { x <- liftEffect e; ... }` / `do { r <- ask; ... }`,
          // the common `do { fib <- fork m; ... }` shape inside
          // traverse-style fan-outs, FiberRef reads, and the case of
          // joining an already-completed child. Each fast path applies
          // `next` to the leaf value directly without pushing a K_BIND
          // frame or taking the value/mode round-trip.
          //
          // The inner loop chains consecutive fast-path BINDs without
          // bouncing through the outer switch. A traverse-style chain
          // of forks or joins fires every step here without paying the
          // per-step interrupt / tick / dispatch cost. Nested BINDs are
          // also handled here: when `inner._tag === BIND` the outer
          // bind goes onto the stack as its own K_BIND frame and the
          // loop descends.
          let bindOp = op;
          bindLoop: while (true) {
            const inner = bindOp.op;
            switch (inner._tag) {
              case PURE: {
                const next = bindOp.next(inner.value);
                if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                this.current = next;
                break bindLoop;
              }
              case BIND: {
                // Nested bind: push the outer as a K_BIND frame and
                // descend without leaving this switch.
                this.stack.push(bindOp);
                bindOp = inner;
                continue bindLoop;
              }
              case FORK: {
                const body = inner.op;
                if (body._tag === PURE && _supervisors.length === 0) {
                  const next = bindOp.next(new DoneFiber(M_OK, body.value));
                  if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                  this.current = next;
                  break bindLoop;
                }
                this.frefsOwn = false;
                const child = new Fiber(body, this.env, this.frefs);
                scheduleFiber(child);
                const next = bindOp.next(child);
                if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                this.current = next;
                break bindLoop;
              }
              case JOIN: {
                const target = inner.fiber;
                if (target.queued) {
                  target.queued = false;
                  target.step();
                }
                if (target.status === F_DONE) {
                  if (target.mode === M_OK) {
                    const next = bindOp.next(target.value);
                    if (next._tag === BIND) {
                      bindOp = next;
                      continue bindLoop;
                    }
                    this.current = next;
                    break bindLoop;
                  }
                  // Non-OK completed join: skip K_BIND, unwind.
                  this._installResult(target);
                  break bindLoop;
                }
                // General suspending path.
                this.stack.push(bindOp);
                this.current = inner;
                break bindLoop;
              }
              case SYNC: {
                let v;
                try {
                  v = inner.run();
                } catch (err) {
                  this.value = err;
                  this.mode = M_DIE;
                  break bindLoop;
                }
                const next = bindOp.next(v);
                if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                this.current = next;
                break bindLoop;
              }
              case ASK: {
                const next = bindOp.next(this.env);
                if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                this.current = next;
                break bindLoop;
              }
              case FORK_INLINE: {
                const body = inner.op;
                const bodyTag = body._tag;
                if (_supervisors.length === 0) {
                  if (bodyTag === PURE) {
                    const next = bindOp.next(new DoneFiber(M_OK, body.value));
                    if (next._tag === BIND) {
                      bindOp = next;
                      continue bindLoop;
                    }
                    this.current = next;
                    break bindLoop;
                  }
                  if (bodyTag === SYNC) {
                    let v;
                    let m;
                    try {
                      v = body.run();
                      m = M_OK;
                    } catch (err) {
                      v = err;
                      m = M_DIE;
                    }
                    const next = bindOp.next(new DoneFiber(m, v));
                    if (next._tag === BIND) {
                      bindOp = next;
                      continue bindLoop;
                    }
                    this.current = next;
                    break bindLoop;
                  }
                }
                this.frefsOwn = false;
                const inlineChild = new Fiber(body, this.env, this.frefs);
                inlineChild.step();
                const next = bindOp.next(inlineChild);
                if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                this.current = next;
                break bindLoop;
              }
              case FREF_GET: {
                const ref = inner.ref;
                const m = this.frefs;
                const next = bindOp.next(
                  (m !== null && m.has(ref)) ? m.get(ref) : ref.initial
                );
                if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                this.current = next;
                break bindLoop;
              }
              case FORK_ALL: {
                // Synchronous fan-out: walk the input array, spawn one
                // fiber per op (DoneFiber for PURE leaves), and apply
                // `next` to the result array without a K_BIND frame.
                const ops = inner.ops;
                const n = ops.length;
                const out = new Array(n);
                if (n > 0) {
                  this.frefsOwn = false;
                  const supEmpty = _supervisors.length === 0;
                  for (let i = 0; i < n; i++) {
                    const body = ops[i];
                    if (body._tag === PURE && supEmpty) {
                      out[i] = new DoneFiber(M_OK, body.value);
                    } else {
                      const child = new Fiber(body, this.env, this.frefs);
                      scheduleFiber(child);
                      out[i] = child;
                    }
                  }
                }
                const next = bindOp.next(out);
                if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                this.current = next;
                break bindLoop;
              }
              case JOIN_ALL: {
                // Try the fully-synchronous fast path: drive any queued
                // siblings inline and collect their results. If they all
                // complete OK we apply `next` here; if one fails we
                // unwind; otherwise we fall back to the outer suspending
                // handler.
                const fibers = inner.fibers;
                const n = fibers.length;
                if (n === 0) {
                  const next = bindOp.next([]);
                  if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                  this.current = next;
                  break bindLoop;
                }
                const results = new Array(n);
                let pending = n;
                let failed = false;
                for (let i = 0; i < n; i++) {
                  const target = fibers[i];
                  if (target.queued) {
                    target.queued = false;
                    target.step();
                  }
                  if (target.status === F_DONE) {
                    if (target.mode !== M_OK) {
                      this._installResult(target);
                      failed = true;
                      break;
                    }
                    results[i] = target.value;
                    pending--;
                  }
                }
                if (failed) break bindLoop;
                if (pending === 0) {
                  const next = bindOp.next(results);
                  if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                  this.current = next;
                  break bindLoop;
                }
                // Pending fibers: defer to outer JOIN_ALL via the K_BIND
                // path so the suspend/observer logic stays in one place.
                this.stack.push(bindOp);
                this.current = inner;
                break bindLoop;
              }
              default:
                // No fast path: reuse the bind op as its own K_BIND frame.
                this.stack.push(bindOp);
                this.current = inner;
                break bindLoop;
            }
          }
          // bindLoop exited with `this.current = next` for non-BIND
          // leaf shapes. If `next` is itself a PURE / SYNC leaf, the
          // outer-loop round-trip just sets (value, mode) and unwinds;
          // collapse it here so a `pure x >>= \a -> pure (f a)` style
          // tail doesn't pay an extra tick + dispatch.
          {
            const cur = this.current;
            if (cur !== null) {
              const curTag = cur._tag;
              if (curTag === PURE) {
                this.value = cur.value;
                this.mode = M_OK;
                this.current = null;
              } else if (curTag === SYNC) {
                this.current = null;
                try {
                  this.value = cur.run();
                  this.mode = M_OK;
                } catch (err) {
                  this.value = err;
                  this.mode = M_DIE;
                }
              }
            }
          }
          continue;
        }
        case FAIL:
          this.value = op.error;
          this.mode = M_FAIL;
          break;
        case CATCH:
          // Reuse the op as its own K_CATCH frame; see opCatchAll.
          this.stack.push(op);
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
              const r = makeResult(M_OK, a);
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
              const r = makeResult(M_FAIL, v);
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
          // Share the frefs map with the child; copy-on-write on the
          // next mutation by either side. See Fiber constructor.
          // PURE bodies skip the Fiber + scheduleFiber path; SYNC
          // bodies do not (their effect must wait for the drain so
          // the parent's next op stays observable before they run).
          const body = op.op;
          if (body._tag === PURE && _supervisors.length === 0) {
            this.value = new DoneFiber(M_OK, body.value);
            this.mode = M_OK;
            break;
          }
          this.frefsOwn = false;
          const child = new Fiber(body, this.env, this.frefs);
          scheduleFiber(child);
          this.value = child;
          this.mode = M_OK;
          break;
        }
        case FORK_INLINE: {
          // Same as FORK but drive the child synchronously before we
          // continue. Sync-bodied children complete here; suspending
          // children hand back a live handle just like FORK. Leaf
          // bodies skip the Fiber alloc entirely.
          const body = op.op;
          const bodyTag = body._tag;
          if (_supervisors.length === 0) {
            if (bodyTag === PURE) {
              this.value = new DoneFiber(M_OK, body.value);
              this.mode = M_OK;
              break;
            }
            if (bodyTag === SYNC) {
              let v;
              let m;
              try {
                v = body.run();
                m = M_OK;
              } catch (err) {
                v = err;
                m = M_DIE;
              }
              this.value = new DoneFiber(m, v);
              this.mode = M_OK;
              break;
            }
          }
          this.frefsOwn = false;
          const inlineChild = new Fiber(body, this.env, this.frefs);
          inlineChild.step();
          this.value = inlineChild;
          this.mode = M_OK;
          break;
        }
        case JOIN: {
          const target = op.fiber;
          const self = this;
          if (target.queued) {
            target.queued = false;
            target.step();
          }
          if (target.status === F_DONE) {
            this._installResult(target);
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
          // Reuse the op as its own K_ENSURE frame; see opEnsuring.
          this.stack.push(op);
          this.current = op.action;
          continue;
        case UNINTERRUPTIBLE:
          this.mask++;
          this.stack.push(FRAME_UNMASK);
          this.current = op.op;
          continue;
        case RACE: {
          // Success on either side wins immediately and interrupts the
          // loser. A single failure waits for the other side: if the
          // other succeeds, that success wins; if the other also fails
          // (or got interrupted while failing), the two causes are
          // composed with `Both`. If both sides end in pure interrupt
          // (no failure observed), the race itself is interrupted.
          const self = this;
          this.frefsOwn = false;
          const leftFiber = new Fiber(op.left, this.env, this.frefs);
          const rightFiber = new Fiber(op.right, this.env, this.frefs);
          let settled = false;
          let firstFailure = null; // Cause held while waiting for the other side
          let bothCompleted = 0;
          const onComplete = function (loser) {
            return function (r) {
              if (settled) return;
              const m = r.mode;
              if (m === M_OK) {
                settled = true;
                loser.interrupt();
                self._resumeAsync(r);
                return;
              }
              if (m === M_INTERRUPT) {
                bothCompleted++;
                if (bothCompleted === 2) {
                  settled = true;
                  if (firstFailure === null) {
                    self._resumeAsync(RESULT_INTERRUPT);
                  } else {
                    self._resumeAsync(makeResult(M_CAUSE, firstFailure));
                  }
                }
                return;
              }
              // failure: fail, die, or cause
              const thisCause = resultToCause(r);
              if (firstFailure === null) {
                firstFailure = thisCause;
                bothCompleted++;
                return;
              }
              settled = true;
              self._resumeAsync(
                makeResult(M_CAUSE, causeBoth(firstFailure, thisCause))
              );
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
          this.stack.push(FRAME_PEEL);
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
          this.frefsOwn = false;
          for (let i = 0; i < n; i++) {
            const idx = i;
            const child = new Fiber(fn(items[idx]), this.env, this.frefs);
            fibers[idx] = child;
            child.observe(function (r) {
              if (settled) return;
              if (r.mode === M_OK) {
                results[idx] = r.value;
                pending--;
                if (pending === 0) {
                  settled = true;
                  self._resumeAsync(makeResult(M_OK, results));
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
        case FORK_ALL: {
          // Walk the input array and spawn one fiber per op in a tight
          // JS loop. PURE leaves get a DoneFiber stub; everything else
          // goes through the scheduler exactly as case FORK does.
          // Equivalent to `traverse fork ops` but skips the per-element
          // BIND chain that traverseArrayImpl would build.
          const ops = op.ops;
          const n = ops.length;
          const out = new Array(n);
          if (n === 0) {
            this.value = out;
            this.mode = M_OK;
            break;
          }
          this.frefsOwn = false;
          const supEmpty = _supervisors.length === 0;
          for (let i = 0; i < n; i++) {
            const body = ops[i];
            if (body._tag === PURE && supEmpty) {
              out[i] = new DoneFiber(M_OK, body.value);
            } else {
              const child = new Fiber(body, this.env, this.frefs);
              scheduleFiber(child);
              out[i] = child;
            }
          }
          this.value = out;
          this.mode = M_OK;
          break;
        }
        case FOR_EACH: {
          // Sequential traverse. Run fn(items[0]) and push a single
          // K_FOR_EACH frame; the frame mutates `i` and `results` in
          // place across iterations / resumptions so we pay one frame
          // alloc + one results-array alloc per forEach, regardless of
          // how many elements (or how many of them suspend).
          const items = op.items;
          const n = items.length;
          if (n === 0) {
            this.value = [];
            this.mode = M_OK;
            break;
          }
          const fn = op.fn;
          let firstOp;
          try {
            firstOp = fn(items[0]);
          } catch (err) {
            this.value = err;
            this.mode = M_DIE;
            break;
          }
          this.stack.push({
            _k: K_FOR_EACH,
            fn: fn,
            items: items,
            n: n,
            i: 0,
            results: new Array(n),
          });
          this.current = firstOp;
          continue;
        }
        case JOIN_ALL: {
          // Wait on a batch of pre-forked fibers. Synchronously drive
          // any that are still in the run queue (matches case JOIN's
          // queue-rendezvous fast path), collect successes in place,
          // and propagate the first non-OK outcome we see. If anything
          // is still pending after the sync pass, suspend and register
          // observers for the rest.
          const fibers = op.fibers;
          const n = fibers.length;
          if (n === 0) {
            this.value = [];
            this.mode = M_OK;
            break;
          }
          const self = this;
          const results = new Array(n);
          let pending = n;
          let failed = false;
          for (let i = 0; i < n; i++) {
            const target = fibers[i];
            if (target.queued) {
              target.queued = false;
              target.step();
            }
            if (target.status === F_DONE) {
              if (target.mode !== M_OK) {
                this._installResult(target);
                failed = true;
                break;
              }
              results[i] = target.value;
              pending--;
            }
          }
          if (failed) break;
          if (pending === 0) {
            this.value = results;
            this.mode = M_OK;
            break;
          }
          let settled = false;
          this.status = F_SUSPENDED;
          this.canceller = null;
          for (let j = 0; j < n; j++) {
            const target = fibers[j];
            if (target.status === F_DONE) continue;
            const idx = j;
            target.observe(function (r) {
              if (settled) return;
              if (r.mode === M_OK) {
                results[idx] = r.value;
                pending--;
                if (pending === 0) {
                  settled = true;
                  self._resumeAsync(makeResult(M_OK, results));
                }
                return;
              }
              settled = true;
              self._resumeAsync(r);
            });
          }
          return;
        }
        case FREF_GET: {
          const ref = op.ref;
          const m = this.frefs;
          this.value = (m !== null && m.has(ref)) ? m.get(ref) : ref.initial;
          this.mode = M_OK;
          break;
        }
        case FREF_SET: {
          if (this.frefs === null) {
            this.frefs = new Map();
            this.frefsOwn = true;
          } else if (!this.frefsOwn) {
            this.frefs = new Map(this.frefs);
            this.frefsOwn = true;
          }
          this.frefs.set(op.ref, op.value);
          this.value = undefined;
          this.mode = M_OK;
          break;
        }
        case FREF_MODIFY: {
          const ref = op.ref;
          const m0 = this.frefs;
          const prev = (m0 !== null && m0.has(ref)) ? m0.get(ref) : ref.initial;
          let next;
          try {
            next = op.fn(prev);
          } catch (err) {
            this.value = err;
            this.mode = M_DIE;
            break;
          }
          if (m0 === null) {
            this.frefs = new Map();
            this.frefsOwn = true;
          } else if (!this.frefsOwn) {
            this.frefs = new Map(m0);
            this.frefsOwn = true;
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
      // Tick budget also applies during unwind: a chain of K_BIND ->
      // BIND continuations can be just as long as a forward-dispatched
      // chain (e.g. the `busy` synthetic stress builds a deep stack
      // that is unwound in one go), and the K_BIND fast paths below
      // can fold leaf ops inline without re-entering the outer dispatch
      // switch.
      if (--ticks < 0) {
        scheduleFiber(this);
        return;
      }
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
            // Inline the common `do { x <- m; pure (f x) }` /
            // `do { x <- m; liftEffect e }` tails: if next returns
            // a leaf op we apply it here without going back through
            // the dispatch switch. The K_BIND -> BIND case is the
            // typical traverse-built shape, so we descend into the
            // BIND inner loop directly to skip the outer dispatch
            // round-trip per nested bind.
            const nextOp = frame.next(this.value);
            const nextTag = nextOp._tag;
            if (nextTag === PURE) {
              this.value = nextOp.value;
              // mode is already M_OK
            } else if (nextTag === SYNC) {
              try {
                this.value = nextOp.run();
              } catch (err) {
                this.value = err;
                this.mode = M_DIE;
              }
            } else if (nextTag === BIND) {
              // Mirror case BIND's inner-loop fast paths so a chain
              // of K_BIND -> BIND continuations doesn't bounce through
              // the outer-loop tick/interrupt/dispatch trio per step.
              let bindOp = nextOp;
              kbindLoop: while (true) {
                const inner = bindOp.op;
                switch (inner._tag) {
                  case PURE: {
                    const next = bindOp.next(inner.value);
                    if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                    this.current = next;
                    break kbindLoop;
                  }
                  case BIND: {
                    this.stack.push(bindOp);
                    bindOp = inner;
                    continue kbindLoop;
                  }
                  case FORK: {
                    const body = inner.op;
                    if (body._tag === PURE && _supervisors.length === 0) {
                      const next = bindOp.next(new DoneFiber(M_OK, body.value));
                      if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                      this.current = next;
                      break kbindLoop;
                    }
                    this.frefsOwn = false;
                    const child = new Fiber(body, this.env, this.frefs);
                    scheduleFiber(child);
                    const next = bindOp.next(child);
                    if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                    this.current = next;
                    break kbindLoop;
                  }
                  case JOIN: {
                    const target = inner.fiber;
                    if (target.queued) {
                      target.queued = false;
                      target.step();
                    }
                    if (target.status === F_DONE) {
                      if (target.mode === M_OK) {
                        const next = bindOp.next(target.value);
                        if (next._tag === BIND) {
                          bindOp = next;
                          continue kbindLoop;
                        }
                        this.current = next;
                        break kbindLoop;
                      }
                      this._installResult(target);
                      break kbindLoop;
                    }
                    this.stack.push(bindOp);
                    this.current = inner;
                    break kbindLoop;
                  }
                  case SYNC: {
                    let v;
                    try {
                      v = inner.run();
                    } catch (err) {
                      this.value = err;
                      this.mode = M_DIE;
                      break kbindLoop;
                    }
                    const next = bindOp.next(v);
                    if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                    this.current = next;
                    break kbindLoop;
                  }
                  case ASK: {
                    const next = bindOp.next(this.env);
                    if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                    this.current = next;
                    break kbindLoop;
                  }
                  case FORK_INLINE: {
                    const body = inner.op;
                    const bodyTag = body._tag;
                    if (_supervisors.length === 0) {
                      if (bodyTag === PURE) {
                        const next = bindOp.next(new DoneFiber(M_OK, body.value));
                        if (next._tag === BIND) {
                          bindOp = next;
                          continue kbindLoop;
                        }
                        this.current = next;
                        break kbindLoop;
                      }
                      if (bodyTag === SYNC) {
                        let v;
                        let m;
                        try {
                          v = body.run();
                          m = M_OK;
                        } catch (err) {
                          v = err;
                          m = M_DIE;
                        }
                        const next = bindOp.next(new DoneFiber(m, v));
                        if (next._tag === BIND) {
                          bindOp = next;
                          continue kbindLoop;
                        }
                        this.current = next;
                        break kbindLoop;
                      }
                    }
                    this.frefsOwn = false;
                    const inlineChild = new Fiber(body, this.env, this.frefs);
                    inlineChild.step();
                    const next = bindOp.next(inlineChild);
                    if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                    this.current = next;
                    break kbindLoop;
                  }
                  case FORK_ALL: {
                    const ops = inner.ops;
                    const n = ops.length;
                    const out = new Array(n);
                    if (n > 0) {
                      this.frefsOwn = false;
                      const supEmpty = _supervisors.length === 0;
                      for (let i = 0; i < n; i++) {
                        const body = ops[i];
                        if (body._tag === PURE && supEmpty) {
                          out[i] = new DoneFiber(M_OK, body.value);
                        } else {
                          const child = new Fiber(body, this.env, this.frefs);
                          scheduleFiber(child);
                          out[i] = child;
                        }
                      }
                    }
                    const next = bindOp.next(out);
                    if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                    this.current = next;
                    break kbindLoop;
                  }
                  case JOIN_ALL: {
                    const fibers = inner.fibers;
                    const n = fibers.length;
                    if (n === 0) {
                      const next = bindOp.next([]);
                      if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                      this.current = next;
                      break kbindLoop;
                    }
                    const results = new Array(n);
                    let pending = n;
                    let failed = false;
                    for (let i = 0; i < n; i++) {
                      const target = fibers[i];
                      if (target.queued) {
                        target.queued = false;
                        target.step();
                      }
                      if (target.status === F_DONE) {
                        if (target.mode !== M_OK) {
                          this._installResult(target);
                          failed = true;
                          break;
                        }
                        results[i] = target.value;
                        pending--;
                      }
                    }
                    if (failed) break kbindLoop;
                    if (pending === 0) {
                      const next = bindOp.next(results);
                      if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                      this.current = next;
                      break kbindLoop;
                    }
                    this.stack.push(bindOp);
                    this.current = inner;
                    break kbindLoop;
                  }
                  default:
                    this.stack.push(bindOp);
                    this.current = inner;
                    break kbindLoop;
                }
              }
              // kbindLoop terminated with `this.current = next` for a
              // non-BIND leaf. If `next` is itself a PURE / SYNC, fold
              // its effect in here so the unwind keeps popping K_BIND
              // frames without an outer step()-loop round-trip per leaf.
              // The unwind loop's own tick decrement keeps preemption
              // honest across long K_BIND chains.
              {
                const cur = this.current;
                if (cur !== null) {
                  const curTag = cur._tag;
                  if (curTag === PURE) {
                    this.value = cur.value;
                    this.current = null;
                    // mode is already M_OK
                  } else if (curTag === SYNC) {
                    this.current = null;
                    try {
                      this.value = cur.run();
                    } catch (err) {
                      this.value = err;
                      this.mode = M_DIE;
                    }
                  }
                }
              }
            } else {
              this.current = nextOp;
            }
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
        case K_FOR_EACH: {
          if (this.mode === M_OK) {
            const i = frame.i;
            frame.results[i] = this.value;
            const nextI = i + 1;
            if (nextI < frame.n) {
              frame.i = nextI;
              let nextOp;
              try {
                nextOp = frame.fn(frame.items[nextI]);
              } catch (err) {
                this.value = err;
                this.mode = M_DIE;
                break;
              }
              this.stack.push(frame);
              this.current = nextOp;
              this.value = null;
            } else {
              this.value = frame.results;
            }
          }
          // FAIL / DIE / INTERRUPT: discard the partial results and
          // bubble out so outer frames see the failure.
          break;
        }
        case K_PEEL: {
          // Capture the current (mode, value) as a tagged result and
          // continue as a success. Clear the interrupt flag so the
          // captured outcome is the final word; if the caller wants
          // to re-propagate the interrupt they can do it from the
          // returned tagged result.
          this.value = this.mode === M_INTERRUPT
            ? RESULT_INTERRUPT
            : makeResult(this.mode, this.value);
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

// Microtask scheduler. Batches every scheduled fiber into a single
// drain so a fan-out of N fibers pays one queueMicrotask dispatch
// instead of N. Fibers can also be drained inline by JOIN before the
// drain runs (the `queued` flag is the rendezvous: whichever side
// clears it first wins, the other side becomes a no-op).
const _runQueue = [];
let _drainScheduled = false;

const _queueDrain =
  typeof queueMicrotask !== "undefined"
    ? function (cb) { queueMicrotask(cb); }
    : function (cb) { setTimeout(cb, 0); };

function _runDrain() {
  _drainScheduled = false;
  // Use an index walk: new fibers scheduled during a step() get
  // appended to _runQueue and picked up in this same drain.
  let i = 0;
  while (i < _runQueue.length) {
    const f = _runQueue[i];
    i++;
    if (f.queued) {
      f.queued = false;
      f.step();
    }
    // else: JOIN drove it inline; nothing to do.
  }
  _runQueue.length = 0;
}

function scheduleFiber(f) {
  f.queued = true;
  _runQueue.push(f);
  if (!_drainScheduled) {
    _drainScheduled = true;
    _queueDrain(_runDrain);
  }
}

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

// Tagged-result inspectors used by the PureScript wrapper. All
// result objects use the uniform { mode, value } shape so each
// predicate is a single field read against a small integer.
export const _resultIsOk = function (r) {
  return r.mode === M_OK;
};

export const _resultIsFail = function (r) {
  return r.mode === M_FAIL;
};

export const _resultIsInterrupted = function (r) {
  return r.mode === M_INTERRUPT;
};

export const _resultIsCause = function (r) {
  return r.mode === M_CAUSE;
};

export const _resultOk = function (r) {
  return r.value;
};

export const _resultFail = function (r) {
  return r.value;
};

export const _resultDie = function (r) {
  return r.value;
};

export const _resultCause = function (r) {
  return r.value;
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
