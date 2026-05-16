"use strict";

// Tags for the Instr ADT. Small integers so V8 can compile the
// dispatch switch to a jump table.
const PURE = 0;
const SYNC = 1;
const FLATMAP = 2;
const ASK = 3;
const FAIL = 4;
const CATCH = 5;
const LOCAL = 6;
const ASYNC = 7;

// Singleton ASK node: there's only ever one shape and no payload,
// so we can reuse the same object instead of allocating per call.
const ASK_NODE = { tag: ASK };

export const instrPure = function (a) {
  return { tag: PURE, value: a };
};

export const instrLiftEffect = function (eff) {
  return { tag: SYNC, eff: eff };
};

export const instrFlatMap = function (m) {
  return function (k) {
    return { tag: FLATMAP, m: m, k: k };
  };
};

export const instrAsk = ASK_NODE;

export const instrFail = function (v) {
  return { tag: FAIL, value: v };
};

// catchTag: wrap `m` in a catch frame. The interpreter records
// the current bind-stack depth at entry and snapshots the env;
// on a matching FAIL it truncates the bind stack, restores env,
// and invokes `handler` with the unwrapped value.
//
// Depends on Data.Variant's runtime shape: VariantRep is
// `{ type: String, value: a }`. See variant-8.0.0/src/Data/
// Variant/Internal.purs (`VariantRep`).
export const _instrCatchTag = function (label) {
  return function (handler) {
    return function (m) {
      return { tag: CATCH, label: label, handler: handler, m: m };
    };
  };
};

// instrLocal: run `inner` with the env transformed by `modify`.
export const _instrLocal = function (modify) {
  return function (inner) {
    return { tag: LOCAL, modify: modify, inner: inner };
  };
};

// instrAsync: suspend the interpreter to run an Aff. The Aff's
// value becomes the result of this instruction. Aff exceptions
// propagate up through the driving loop in PureScript.
//
// Note: this is the bridge that makes the spike useful for real
// programs. liftAff / liftEffect on top of Aff and async work
// (fork, timeouts, network IO) all flow through here.
export const instrAsync = function (aff) {
  return { tag: ASYNC, aff: aff };
};

// Fresh mutable interpreter state.
export const _initInstrState = function (env) {
  return function (initial) {
    return function () {
      return {
        stack: [],
        catches: [],
        envs: [],
        env: env,
        current: initial,
        result: undefined,
        pendingAff: null,
        done: false,
        finalRight: null, // { value: a } when success
        finalLeft: null, // { variant: V } when typed failure
      };
    };
  };
};

// Step the interpreter until it suspends on ASYNC, completes
// with a value, or fails with an unhandled typed failure.
//
// Mutates `state` in place. The caller drives the loop:
//
//   1. Call _stepInstr.
//   2. If state.done, read finalRight or finalLeft.
//   3. Otherwise, state.pendingAff is set; the caller runs it,
//      passes the result to _resumeInstr, and loops.
export const _stepInstr = function (state) {
  return function () {
    while (true) {
      if (state.current === null) {
        if (state.stack.length === 0) {
          state.done = true;
          state.finalRight = { value: state.result };
          return;
        }
        const k = state.stack.pop();
        while (
          state.envs.length > 0 &&
          state.envs[state.envs.length - 1].depth > state.stack.length
        ) {
          state.env = state.envs.pop().env;
        }
        while (
          state.catches.length > 0 &&
          state.catches[state.catches.length - 1].depth > state.stack.length
        ) {
          state.catches.pop();
        }
        state.current = k(state.result);
        continue;
      }

      switch (state.current.tag) {
        case 0: // PURE
          state.result = state.current.value;
          state.current = null;
          break;
        case 1: // SYNC
          state.result = state.current.eff();
          state.current = null;
          break;
        case 2: // FLATMAP
          state.stack.push(state.current.k);
          state.current = state.current.m;
          break;
        case 3: // ASK
          state.result = state.env;
          state.current = null;
          break;
        case 4: { // FAIL
          const variant = state.current.value;
          const label = variant.type;
          let matched = -1;
          for (let i = state.catches.length - 1; i >= 0; i--) {
            if (state.catches[i].label === label) {
              matched = i;
              break;
            }
          }
          if (matched === -1) {
            state.done = true;
            state.finalLeft = { variant: variant };
            return;
          }
          const frame = state.catches[matched];
          state.stack.length = frame.depth;
          state.catches.length = matched;
          while (
            state.envs.length > 0 &&
            state.envs[state.envs.length - 1].depth > frame.depth
          ) {
            state.envs.pop();
          }
          state.env = frame.envSnapshot;
          state.current = frame.handler(variant.value);
          state.result = undefined;
          break;
        }
        case 5: // CATCH
          state.catches.push({
            depth: state.stack.length,
            label: state.current.label,
            handler: state.current.handler,
            envSnapshot: state.env,
          });
          state.current = state.current.m;
          break;
        case 6: // LOCAL
          state.envs.push({ depth: state.stack.length, env: state.env });
          state.env = state.current.modify(state.env);
          state.current = state.current.inner;
          break;
        case 7: // ASYNC
          state.pendingAff = state.current.aff;
          state.current = null;
          return;
        default:
          throw new Error("Benchmarks.Instr: unknown tag " + state.current.tag);
      }
    }
  };
};

// After an Aff completed with `value`, install it as the next
// result and clear the pending slot so the next step continues.
export const _resumeInstr = function (state) {
  return function (value) {
    return function () {
      state.result = value;
      state.pendingAff = null;
    };
  };
};

// Accessors used by the PureScript driver.
export const _isDone = function (state) {
  return state.done;
};

export const _isRightFinal = function (state) {
  return state.finalRight !== null;
};

export const _finalRight = function (state) {
  return state.finalRight.value;
};

export const _finalLeft = function (state) {
  return state.finalLeft.variant;
};

export const _pendingAff = function (state) {
  return state.pendingAff;
};
