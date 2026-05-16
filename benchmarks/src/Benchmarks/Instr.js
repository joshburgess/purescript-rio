"use strict";

// Tags for the Instr ADT. Small integers so V8 can compile the
// dispatch switch to a jump table.
const PURE = 0;
const SYNC = 1;
const FLATMAP = 2;
const ASK = 3;
const FAIL = 4;
const CATCH = 5;

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
// the current bind-stack depth at entry; if a FAIL surfaces
// inside `m` whose Variant `type` field matches `label`, the
// interpreter truncates the bind stack back to that depth and
// invokes `handler` with the unwrapped value.
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

// The interpreter.
//
// Two parallel stacks:
//   - `stack`  : bind continuations (functions `a -> Instr`).
//   - `catches`: catch frames {depth, label, handler}, where
//                `depth` is the bind-stack length at frame entry.
//
// Hot path (bind / sync / pure / ask): the catches array is
// only consulted on continuation pop and on FAIL, so the common
// case pays at most one length-zero check.
//
// On normal value propagation we drop any catch frames whose
// protected scope has now been exited (the bind stack has
// dropped below the frame's recorded depth).
//
// On FAIL we walk `catches` from the top looking for a matching
// label. We truncate the bind stack to the matched frame's depth
// and resume by stepping into `handler(value)`. If no frame
// matches, we terminate the computation with `Left variant`.
export const _runInstr = function (left) {
  return function (right) {
    return function (env) {
      return function (initial) {
        return function () {
          const stack = [];
          const catches = [];
          let current = initial;
          let result = undefined;

          while (true) {
            if (current === null) {
              if (stack.length === 0) {
                return right(result);
              }
              const k = stack.pop();
              // Drop any catch frames whose scope is now exited.
              while (
                catches.length > 0 &&
                catches[catches.length - 1].depth > stack.length
              ) {
                catches.pop();
              }
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
              case 4: { // FAIL
                const variant = current.value;
                const label = variant.type;
                let matched = -1;
                for (let i = catches.length - 1; i >= 0; i--) {
                  if (catches[i].label === label) {
                    matched = i;
                    break;
                  }
                }
                if (matched === -1) {
                  return left(variant);
                }
                const frame = catches[matched];
                stack.length = frame.depth;
                catches.length = matched;
                current = frame.handler(variant.value);
                result = undefined;
                break;
              }
              case 5: // CATCH
                catches.push({
                  depth: stack.length,
                  label: current.label,
                  handler: current.handler,
                });
                current = current.m;
                break;
              default:
                throw new Error("Benchmarks.Instr: unknown tag " + current.tag);
            }
          }
        };
      };
    };
  };
};
