"use strict";

// Internal interpreter for the rio-fiber prototype.
//
// `Op` is a tagged-object instruction tree built by the FFI factories
// below. The step loop in `_runFiber` walks the tree iteratively,
// pushing continuation frames onto a stack so PureScript's stack
// usage stays flat. This is the well-known fiber-runtime shape used
// by Effect / ZIO / Cats Effect; we start synchronous and grow the
// runtime in subsequent phases.

// Op tags. Stored as small integers so the dispatch can be a single
// `switch` on a hot field.
const PURE = 0;
const SYNC = 1;
const BIND = 2;
const FAIL = 3;
const CATCH = 4;
const ASK = 5;
const LOCAL = 6;

// Continuation-stack frame tags.
const K_BIND = 0; // next: a -> Op r e b
const K_CATCH = 1; // handler: Variant e -> Op r e' a
const K_LOCAL = 2; // restore env: previous env to put back

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

// Runner ---------------------------------------------------------------
//
// `_runFiber rio env` returns an `Effect` (a JS thunk) that produces
// a small tagged result object:
//
//   { ok: a }            -- success
//   { fail: variant }    -- typed failure (Variant e)
//   { die: error }       -- defect (any JS throwable)
//
// The PureScript wrapper in Internal.purs turns this into
// `Either (Either Error (Variant e)) a`. Keeping the JS side free of
// PureScript constructors keeps the FFI surface trivial.

export const _runFiber = function (rio) {
  return function (env0) {
    return function () {
      // newtype unwrap: RIO r e a = RIO (Op r e a). PureScript's
      // newtype runtime rep is the underlying value directly, so
      // `rio` IS the Op already - no `.value0` needed.
      let current = rio;
      let value = null;
      let mode = "ok";
      const stack = [];
      let env = env0;

      while (true) {
        if (current !== null) {
          const op = current;
          current = null;
          switch (op._tag) {
            case PURE:
              value = op.value;
              mode = "ok";
              break;
            case SYNC:
              try {
                value = op.run();
                mode = "ok";
              } catch (err) {
                return { die: err };
              }
              break;
            case BIND:
              stack.push({ _k: K_BIND, next: op.next });
              current = op.op;
              continue;
            case FAIL:
              value = op.error;
              mode = "fail";
              break;
            case CATCH:
              stack.push({ _k: K_CATCH, handler: op.handler });
              current = op.op;
              continue;
            case ASK:
              value = env;
              mode = "ok";
              break;
            case LOCAL: {
              const prev = env;
              env = op.transform(env);
              stack.push({ _k: K_LOCAL, prev: prev });
              current = op.op;
              continue;
            }
            default:
              return {
                die: new Error("rio-fiber: unknown Op tag " + op._tag),
              };
          }
        }

        // No current op; unwind continuation frames until we hit one
        // that consumes our (value, mode) or until the stack drains.
        while (true) {
          if (stack.length === 0) {
            if (mode === "ok") return { ok: value };
            return { fail: value };
          }
          const frame = stack.pop();
          if (frame._k === K_BIND) {
            if (mode === "ok") {
              current = frame.next(value);
              break;
            }
            // failure passes through bind frames; keep unwinding.
          } else if (frame._k === K_CATCH) {
            if (mode === "fail") {
              current = frame.handler(value);
              mode = "ok"; // entering the recovery path
              break;
            }
            // success bypasses the handler; keep unwinding.
          } else {
            // K_LOCAL: restore env unconditionally and keep unwinding.
            env = frame.prev;
          }
        }
      }
    };
  };
};

// Tagged-result inspectors used by the PureScript wrapper.
export const _resultIsOk = function (r) {
  return Object.prototype.hasOwnProperty.call(r, "ok");
};

export const _resultIsFail = function (r) {
  return Object.prototype.hasOwnProperty.call(r, "fail");
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
