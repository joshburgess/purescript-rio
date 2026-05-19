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

// Continuation-stack frame tags.
const K_BIND = 0; // next: a -> Op r e b
const K_CATCH = 1; // handler: Variant e -> Op r e' a
const K_LOCAL = 2; // restore env: previous env to put back

// Fiber statuses.
const F_RUNNING = 0;
const F_SUSPENDED = 1;
const F_DONE = 2;

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

// Fiber ----------------------------------------------------------------
//
// One Fiber per RIO program execution. Started fibers run their step
// loop synchronously to completion or suspension; on suspension the
// loop yields control and resumes through a callback.

function Fiber(op, env) {
  this.current = op;
  this.value = null;
  this.mode = "ok"; // "ok" carries a success; "fail" carries a Variant e
  this.stack = [];
  this.env = env;
  this.status = F_RUNNING;
  this.result = null; // { ok } | { fail } | { die } | { interrupted: true }
  this.observers = [];
  this.interrupted = false;
  this.canceller = null;
}

Fiber.prototype._complete = function (result) {
  if (this.status === F_DONE) return;
  this.status = F_DONE;
  this.result = result;
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
    this._complete({ interrupted: true });
  }
  // If F_RUNNING the step loop will see the flag and unwind.
};

// Install the result of a completed async / join into the fiber's
// (value, mode). Returns true if the fiber should continue stepping,
// false if it has already completed (defect or interrupt propagation).
Fiber.prototype._installResult = function (r) {
  if (Object.prototype.hasOwnProperty.call(r, "die")) {
    this._complete({ die: r.die });
    return false;
  }
  if (r.interrupted) {
    this.interrupted = true;
    return true;
  }
  if (Object.prototype.hasOwnProperty.call(r, "ok")) {
    this.value = r.ok;
    this.mode = "ok";
    return true;
  }
  // fail
  this.value = r.fail;
  this.mode = "fail";
  return true;
};

Fiber.prototype.step = function () {
  while (true) {
    if (this.interrupted) {
      this._complete({ interrupted: true });
      return;
    }
    if (this.current !== null) {
      const op = this.current;
      this.current = null;
      switch (op._tag) {
        case PURE:
          this.value = op.value;
          this.mode = "ok";
          break;
        case SYNC:
          try {
            this.value = op.run();
            this.mode = "ok";
          } catch (err) {
            this._complete({ die: err });
            return;
          }
          break;
        case BIND:
          this.stack.push({ _k: K_BIND, next: op.next });
          this.current = op.op;
          continue;
        case FAIL:
          this.value = op.error;
          this.mode = "fail";
          break;
        case CATCH:
          this.stack.push({ _k: K_CATCH, handler: op.handler });
          this.current = op.op;
          continue;
        case ASK:
          this.value = this.env;
          this.mode = "ok";
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
          // PureScript curried: register(onOk)(onFail)() -> canceller.
          const onOk = function (a) {
            return function () {
              if (settled) return;
              settled = true;
              const r = { ok: a };
              if (self.status === F_RUNNING) {
                // Resumed synchronously inside register(): record and
                // fall through after register returns.
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
            this._complete({ die: err });
            return;
          }
          if (syncResult !== null) {
            // Resumed during register(); continue with that result.
            if (!this._installResult(syncResult)) return;
            break;
          }
          this.status = F_SUSPENDED;
          this.canceller = canceller;
          return;
        }
        case FORK: {
          const child = new Fiber(op.op, this.env);
          scheduleFiber(child);
          this.value = child;
          this.mode = "ok";
          break;
        }
        case JOIN: {
          const target = op.fiber;
          const self = this;
          if (target.status === F_DONE) {
            if (!this._installResult(target.result)) return;
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
          this.mode = "ok";
          break;
        }
        default:
          this._complete({
            die: new Error("rio-fiber: unknown Op tag " + op._tag),
          });
          return;
      }
    }

    // No current op; unwind continuation frames.
    while (true) {
      if (this.interrupted) {
        this._complete({ interrupted: true });
        return;
      }
      if (this.stack.length === 0) {
        if (this.mode === "ok") {
          this._complete({ ok: this.value });
          return;
        }
        this._complete({ fail: this.value });
        return;
      }
      const frame = this.stack.pop();
      if (frame._k === K_BIND) {
        if (this.mode === "ok") {
          this.current = frame.next(this.value);
          break;
        }
      } else if (frame._k === K_CATCH) {
        if (this.mode === "fail") {
          this.current = frame.handler(this.value);
          this.mode = "ok";
          break;
        }
      } else {
        this.env = frame.prev;
      }
    }
  }
};

Fiber.prototype._resumeAsync = function (r) {
  this.canceller = null;
  this.status = F_RUNNING;
  if (!this._installResult(r)) return;
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

// Start a fiber and run its step loop until completion or suspension.
// The returned object is the Fiber itself, with `status` / `result`
// readable by the PureScript wrappers.
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

export const _resultOk = function (r) {
  return r.ok;
};

export const _resultFail = function (r) {
  return r.fail;
};

export const _resultDie = function (r) {
  return r.die;
};
