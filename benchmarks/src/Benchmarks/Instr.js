"use strict";

// Tags for the Instr ADT. Small integers so V8 can compile the
// dispatch switch to a jump table.
const PURE = 0;
const SYNC = 1;
const FLATMAP = 2;
const ASK = 3;
const FAIL = 4;

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

// The interpreter.
//
// Convention: `current` holds the next instruction to interpret, or
// `null` when we've produced a value sitting in `result`.
//
// `stack` holds the continuation chain for FLATMAP: when we step
// into the `m` side of a FLATMAP, we push the `k` onto the stack;
// when a node terminates with a value, we pop and apply.
//
// Failures short-circuit out of the loop by returning a Left
// immediately; the production version would walk the stack looking
// for a catch frame but this spike doesn't need that.
export const _runInstr = function (left) {
  return function (right) {
    return function (env) {
      return function (initial) {
        return function () {
          const stack = [];
          let current = initial;
          let result = undefined;

          while (true) {
            if (current === null) {
              // We have a value in `result`; resume the next
              // continuation if any.
              if (stack.length === 0) {
                return right(result);
              }
              const k = stack.pop();
              current = k(result);
              continue;
            }

            switch (current.tag) {
              case 0: // PURE
                result = current.value;
                current = null;
                break;
              case 1: // SYNC (liftEffect)
                result = current.eff();
                current = null;
                break;
              case 2: // FLATMAP
                stack.push(current.k);
                current = current.m;
                break;
              case 3: // ASK
                result = env;
                current = null;
                break;
              case 4: // FAIL
                return left(current.value);
              default:
                throw new Error("Benchmarks.Instr: unknown tag " + current.tag);
            }
          }
        };
      };
    };
  };
};
