"use strict";

// Marker key attached to Error instances that carry an RIO typed
// failure. Anything stored under this key is treated as a Variant
// payload.
const RIO_TYPED_FAILURE = "__rio$typedFailure";

export const _mkTypedFailure = function (variant) {
  // `new Error` with a fixed message keeps stack traces lightweight
  // while still letting `instanceof Error` work for downstream code.
  const e = new Error("RIO typed failure");
  e[RIO_TYPED_FAILURE] = variant;
  return e;
};

export const _matchTypedFailure = function (nothing) {
  return function (just) {
    return function (err) {
      if (err !== null && typeof err === "object" && RIO_TYPED_FAILURE in err) {
        return just(err[RIO_TYPED_FAILURE]);
      } else {
        return nothing;
      }
    };
  };
};

// ---------------------------------------------------------------------------
// Instruction-list interpreter.
//
// Tags are small integers so V8 compiles the dispatch switch to a
// jump table.
// ---------------------------------------------------------------------------

const PURE = 0;
const SYNC = 1;
const FLATMAP = 2;
const ASK = 3;
const FAIL = 4;
const CATCH = 5;
const LOCAL = 6;
const ASYNC = 7;
const LIFT = 8;
const SYNC_LIFT = 9;

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

export const instrCatchTag = function (label) {
  return function (handler) {
    return function (m) {
      return { tag: CATCH, label: label, handler: handler, m: m };
    };
  };
};

export const instrLocal = function (modify) {
  return function (inner) {
    return { tag: LOCAL, modify: modify, inner: inner };
  };
};

export const instrLiftAff = function (aff) {
  return { tag: ASYNC, aff: aff };
};

// LIFT: the env-aware `Record r -> Aff a` bridge. The interpreter
// evaluates `fn(env)` to obtain the Aff at suspension time.
export const instrLift = function (fn) {
  return { tag: LIFT, fn: fn };
};

// SYNC_LIFT: env-aware Effect bridge. Like LIFT but the closure
// produces an Effect; the interpreter runs it synchronously inside
// the inner loop, no Aff suspension. The dominant `mkRIO \r ->
// liftEffect (...)` shape compiles to one of these.
export const instrSyncLift = function (fn) {
  return { tag: SYNC_LIFT, fn: fn };
};

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
        finalRight: null,
        finalLeft: null,
      };
    };
  };
};

const runLoop = function (state) {
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
      case 8: // LIFT
        state.pendingAff = state.current.fn(state.env);
        state.current = null;
        return;
      case 9: // SYNC_LIFT
        state.result = state.current.fn(state.env)();
        state.current = null;
        break;
      default:
        throw new Error("RIO.Internal: unknown instruction tag " + state.current.tag);
    }
  }
};

export const _stepInstr = function (state) {
  return function () {
    runLoop(state);
  };
};

// Combined resume + step. Installs `value` as the next result and
// immediately continues the inner loop.
export const _resumeAndStep = function (state) {
  return function (value) {
    return function () {
      state.result = value;
      state.pendingAff = null;
      runLoop(state);
    };
  };
};

// Inject a FAIL into the interpreter and continue. Called by the
// driver when the suspended Aff threw a tagged typed failure.
export const _failAndStep = function (state) {
  return function (variant) {
    return function () {
      state.pendingAff = null;
      state.current = { tag: FAIL, value: variant };
      runLoop(state);
    };
  };
};

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
