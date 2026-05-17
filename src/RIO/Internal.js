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
const BIND = 2;
const ASK = 3;
const FAIL = 4;
const CATCH = 5;
const LOCAL = 6;
const ASYNC = 7;
const LIFT = 8;
const SYNC_LIFT = 9;
const CATCH_ALL = 10;

// Singleton ASK node: there's only ever one shape and no payload,
// so we can reuse the same object instead of allocating per call.
const ASK_NODE = { tag: ASK };

export const instrPure = function (a) {
  return { tag: PURE, value: a };
};

export const instrLiftEffect = function (eff) {
  return { tag: SYNC, eff: eff };
};

export const instrBind = function (m) {
  return function (k) {
    return { tag: BIND, m: m, k: k };
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

// CATCH_ALL: like CATCH but matches any typed failure. The handler
// receives the full `Variant e` rather than just a payload, mirroring
// the `catchAll` surface combinator's contract. Defects (untagged
// Aff exceptions) are NOT caught - they keep propagating just as in
// the old mkRIO+attempt implementation.
export const instrCatchAll = function (handler) {
  return function (m) {
    return { tag: CATCH_ALL, handler: handler, m: m };
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

// Fast-path predicate: top-level node is PURE, so the result is its
// `value` field and no state allocation / interpreter loop is needed.
// Hot path inside parTraverse / sequence-style combinators where each
// element is `pure x`.
export const _isPureInstr = function (instr) {
  return instr.tag === PURE;
};

// UNSAFE: only valid when `_isPureInstr instr` is true.
export const _purePayload = function (instr) {
  return instr.value;
};

// Fast-path predicate: top-level node is SYNC (liftEffect). The result
// is `eff()` and no state allocation / interpreter loop is needed.
// Common in parTraverse over Effect-heavy work.
export const _isSyncInstr = function (instr) {
  return instr.tag === SYNC;
};

// UNSAFE: only valid when `_isSyncInstr instr` is true.
export const _syncEff = function (instr) {
  return instr.eff;
};

const PROFILE_ENABLED =
  typeof process !== "undefined" &&
  process.env &&
  process.env.RIO_INSTR_PROFILE === "1";

const _INSTR_COUNTS = new Uint32Array(11);

export const _dumpInstrCounts = function () {
  return {
    PURE: _INSTR_COUNTS[0],
    SYNC: _INSTR_COUNTS[1],
    BIND: _INSTR_COUNTS[2],
    ASK: _INSTR_COUNTS[3],
    FAIL: _INSTR_COUNTS[4],
    CATCH: _INSTR_COUNTS[5],
    LOCAL: _INSTR_COUNTS[6],
    ASYNC: _INSTR_COUNTS[7],
    LIFT: _INSTR_COUNTS[8],
    SYNC_LIFT: _INSTR_COUNTS[9],
    CATCH_ALL: _INSTR_COUNTS[10],
  };
};

export const _resetInstrCounts = function () {
  _INSTR_COUNTS.fill(0);
};

// Fused pop helper: invoked from PURE / SYNC / ASK / SYNC_LIFT to avoid
// a second loop trip through `current === null`. Returns true when the
// computation has completed (caller should `return`); false means the
// continuation has been installed in `state.current` and dispatch
// should continue.
const popK = function (state, value) {
  if (state.stack.length === 0) {
    state.done = true;
    state.finalRight = { value: value };
    return true;
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
  state.current = k(value);
  return false;
};

const runLoopFast = function (state) {
  while (true) {
    switch (state.current.tag) {
      case 0: // PURE
        if (popK(state, state.current.value)) return;
        break;
      case 1: // SYNC
        if (popK(state, state.current.eff())) return;
        break;
      case 2: // BIND
        state.stack.push(state.current.k);
        state.current = state.current.m;
        break;
      case 3: // ASK
        if (popK(state, state.env)) return;
        break;
      case 4: { // FAIL
        const variant = state.current.value;
        const label = variant.type;
        let matched = -1;
        for (let i = state.catches.length - 1; i >= 0; i--) {
          const frameLabel = state.catches[i].label;
          if (frameLabel === null || frameLabel === label) {
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
        // CATCH_ALL frames (label === null) receive the whole Variant;
        // CATCH (per-tag) frames receive the payload only.
        state.current = frame.label === null
          ? frame.handler(variant)
          : frame.handler(variant.value);
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
        if (popK(state, state.current.fn(state.env)())) return;
        break;
      case 10: // CATCH_ALL
        state.catches.push({
          depth: state.stack.length,
          label: null,
          handler: state.current.handler,
          envSnapshot: state.env,
        });
        state.current = state.current.m;
        break;
      default:
        throw new Error("RIO.Internal: unknown instruction tag " + state.current.tag);
    }
  }
};

const runLoopProfiled = function (state) {
  while (true) {
    _INSTR_COUNTS[state.current.tag]++;

    switch (state.current.tag) {
      case 0:
        if (popK(state, state.current.value)) return;
        break;
      case 1:
        if (popK(state, state.current.eff())) return;
        break;
      case 2:
        state.stack.push(state.current.k);
        state.current = state.current.m;
        break;
      case 3:
        if (popK(state, state.env)) return;
        break;
      case 4: {
        const variant = state.current.value;
        const label = variant.type;
        let matched = -1;
        for (let i = state.catches.length - 1; i >= 0; i--) {
          const frameLabel = state.catches[i].label;
          if (frameLabel === null || frameLabel === label) {
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
        state.current = frame.label === null
          ? frame.handler(variant)
          : frame.handler(variant.value);
        break;
      }
      case 5:
        state.catches.push({
          depth: state.stack.length,
          label: state.current.label,
          handler: state.current.handler,
          envSnapshot: state.env,
        });
        state.current = state.current.m;
        break;
      case 6:
        state.envs.push({ depth: state.stack.length, env: state.env });
        state.env = state.current.modify(state.env);
        state.current = state.current.inner;
        break;
      case 7:
        state.pendingAff = state.current.aff;
        state.current = null;
        return;
      case 8:
        state.pendingAff = state.current.fn(state.env);
        state.current = null;
        return;
      case 9:
        if (popK(state, state.current.fn(state.env)())) return;
        break;
      case 10:
        state.catches.push({
          depth: state.stack.length,
          label: null,
          handler: state.current.handler,
          envSnapshot: state.env,
        });
        state.current = state.current.m;
        break;
      default:
        throw new Error("RIO.Internal: unknown instruction tag " + state.current.tag);
    }
  }
};

const runLoop = PROFILE_ENABLED ? runLoopProfiled : runLoopFast;

export const _stepInstr = function (state) {
  return function () {
    runLoop(state);
  };
};

// Combined resume + step. Installs `value` as the result of the
// suspended instruction, advances past the continuation, and
// continues dispatch. Mirrors the pop-K path inside `popK` because
// the runLoop entry invariant is "state.current is non-null".
export const _resumeAndStep = function (state) {
  return function (value) {
    return function () {
      state.pendingAff = null;
      if (state.stack.length === 0) {
        state.done = true;
        state.finalRight = { value: value };
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
      state.current = k(value);
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
