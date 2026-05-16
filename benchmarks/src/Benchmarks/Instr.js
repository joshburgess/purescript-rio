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
// the current bind-stack depth at entry and snapshots the
// current env; if a FAIL surfaces inside `m` whose Variant
// `type` field matches `label`, the interpreter truncates the
// bind stack back to that depth, restores env to the snapshot,
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
// The interpreter pushes the current env onto the `envs` stack
// (with the current bind-stack depth), sets env to `modify(env)`,
// then steps into `inner`. When the scope exits, the env is
// restored. provide / provideAll are built on top of this.
export const _instrLocal = function (modify) {
  return function (inner) {
    return { tag: LOCAL, modify: modify, inner: inner };
  };
};

// The interpreter.
//
// Three parallel stacks:
//   - `stack`  : bind continuations (functions `a -> Instr`).
//   - `catches`: catch frames {depth, label, handler, envSnapshot}.
//   - `envs`   : env-restore frames {depth, env}.
//
// `depth` records the bind-stack length at frame entry. On
// normal value propagation, frames whose protected scope has
// exited (bind stack dropped below their depth) are popped. For
// `envs` frames the env is also restored on pop.
//
// On FAIL we walk `catches` from the top looking for a matching
// label. We truncate the bind stack to the matched frame's
// depth, restore env from the frame's snapshot, drop any envs
// strictly past the frame's depth, and resume by stepping into
// `handler(value)`. No match terminates the computation with
// `Left variant`.
//
// Hot path (bind / sync / pure / ask / local) stays cheap:
// catches and envs are only consulted on continuation pop and
// on FAIL.
export const _runInstr = function (left) {
  return function (right) {
    return function (initialEnv) {
      return function (initial) {
        return function () {
          const stack = [];
          const catches = [];
          const envs = [];
          let env = initialEnv;
          let current = initial;
          let result = undefined;

          while (true) {
            if (current === null) {
              if (stack.length === 0) {
                return right(result);
              }
              const k = stack.pop();
              // Drop env frames whose scope is exited, restoring env.
              while (
                envs.length > 0 &&
                envs[envs.length - 1].depth > stack.length
              ) {
                env = envs.pop().env;
              }
              // Drop catch frames whose scope is exited.
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
                // Drop env frames strictly past the catch's depth;
                // env is restored from the catch's snapshot.
                while (
                  envs.length > 0 &&
                  envs[envs.length - 1].depth > frame.depth
                ) {
                  envs.pop();
                }
                env = frame.envSnapshot;
                current = frame.handler(variant.value);
                result = undefined;
                break;
              }
              case 5: // CATCH
                catches.push({
                  depth: stack.length,
                  label: current.label,
                  handler: current.handler,
                  envSnapshot: env,
                });
                current = current.m;
                break;
              case 6: // LOCAL
                envs.push({ depth: stack.length, env: env });
                env = current.modify(env);
                current = current.inner;
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
