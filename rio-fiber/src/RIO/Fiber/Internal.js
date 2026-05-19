"use strict";

import { Right as _Right, Left as _Left } from "../Data.Either/index.js";

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
const MAP = 24;
const APPLY = 25;
const FORK_ALL_INLINE = 26;

// Continuation-stack frame tags.
const K_BIND = 0; // next: a -> Op r e b
const K_CATCH = 1; // handler: Variant e -> Op r e' a
const K_LOCAL = 2; // restore env: previous env to put back
const K_ENSURE = 3; // run finalizer regardless of outcome
const K_AFTER_FIN = 4; // restore saved (value, mode) after finalizer
const K_UNMASK = 5; // decrement mask depth
const K_PEEL = 6; // capture the current (mode, value) as a tagged result
const K_FOR_EACH = 7; // sequential traverse: collect this iteration, advance
const K_MAP = 8;   // apply f to the result without an extra PURE wrap
const K_APPLY = 9; // value (f) is in hand, now evaluate the second op
const K_APPLY2 = 10; // both ops complete: apply the captured f to the value

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

// Single hidden class for every Op node AND every continuation frame.
// The constructor always assigns _tag, _k, _1, _2 in the same order,
// so V8 builds one hidden class for the whole instruction set and the
// dispatcher's `op._tag` read stays monomorphic. (This is the same
// trick `Effect.Aff` uses with its `new Aff(tag, _1, _2, _3)`.)
//
// _tag is the dispatch tag (PURE / SYNC / BIND / ...) or -1 for
// frames that aren't also ops (K_MAP, K_APPLY, ...). _k is the
// unwind tag (K_BIND / K_CATCH / ...) or -1 for ops that don't
// double as a frame (PURE / SYNC / FORK / ...). BIND / CATCH /
// ENSURING set both, so the op object can be pushed directly onto
// the stack as its own K_BIND / K_CATCH / K_ENSURE frame, saving
// one allocation per bind / catch / ensure on the hot path. _1 and
// _2 carry the payload; layouts are documented in the smart-
// constructor block below.
function Op(tag, k, _1, _2) {
  this._tag = tag;
  this._k = k;
  this._1 = _1;
  this._2 = _2;
}

// K_FOR_EACH carries five fields (fn, items, n, i, results) plus the
// mutable `i` cursor that the unwind handler advances in place. It
// gets its own class so the four-slot Op shape stays clean.
function ForEachFrame(fn, items, results) {
  this._k = K_FOR_EACH;
  this.fn = fn;
  this.items = items;
  this.n = items.length;
  this.i = 0;
  this.results = results;
}

// Op factories ---------------------------------------------------------
//
// Slot layout (per tag):
//   PURE            : _1 = value
//   SYNC            : _1 = run thunk
//   BIND            : _1 = inner op,        _2 = next continuation
//   FAIL            : _1 = Variant error
//   CATCH           : _1 = inner op,        _2 = handler
//   LOCAL           : _1 = transform fn,    _2 = inner op
//   ASYNC           : _1 = register
//   FORK / FORK_INLINE: _1 = body op
//   JOIN / INTERRUPT  : _1 = target fiber
//   ENSURING        : _1 = action,          _2 = finalizer
//   UNINTERRUPTIBLE : _1 = inner op
//   RACE            : _1 = left op,         _2 = right op
//   PAR_TRAVERSE    : _1 = fn,              _2 = items
//   PEEL            : _1 = inner op
//   FREF_GET        : _1 = ref
//   FREF_SET        : _1 = ref,             _2 = value
//   FREF_MODIFY     : _1 = ref,             _2 = fn
//   FAIL_CAUSE      : _1 = cause
//   FORK_ALL / FORK_ALL_INLINE: _1 = ops array
//   JOIN_ALL        : _1 = fibers array
//   FOR_EACH        : _1 = fn,              _2 = items
//   MAP             : _1 = function,        _2 = inner op
//   APPLY           : _1 = opF,             _2 = opA
//   K_LOCAL frame   : _1 = previous env
//   K_AFTER_FIN     : _1 = savedValue,      _2 = savedMode
//   K_MAP frame     : _1 = function  (a MAP op pushed directly, _k=K_MAP)
//   K_APPLY frame   : _2 = opA       (an APPLY op pushed directly, _k=K_APPLY)
//   K_APPLY2 frame  : _1 = function

export const opPure = function (a) {
  return new Op(PURE, -1, a, null);
};

export const opLiftEffect = function (eff) {
  return new Op(SYNC, -1, eff, null);
};

// The BIND op also doubles as its own K_BIND continuation frame
// (same `_k` field the unwind switch reads, same `_2` carrying the
// continuation). When the step loop encounters a BIND it can push
// the op itself instead of allocating a fresh frame, saving one
// allocation per bind on the hot path.
export const opBind = function (m) {
  return function (k) {
    return new Op(BIND, K_BIND, m, k);
  };
};

// Singleton ASK: no payload, one shape, reuse the object.
const ASK_NODE = new Op(ASK, -1, null, null);
export const opAsk = ASK_NODE;

// Singleton continuation frames that carry no per-instance payload:
// pushing the singleton avoids a per-step allocation. Frames are
// never mutated during the unwind, so sharing is safe.
const FRAME_UNMASK = new Op(-1, K_UNMASK, null, null);
const FRAME_PEEL = new Op(-1, K_PEEL, null, null);

export const opFail = function (e) {
  return new Op(FAIL, -1, e, null);
};

// CATCH op doubles as its own K_CATCH frame (same trick as opBind):
// the op carries `handler` in _2, and the unwind switch only needs
// `_k` and `_2`. Pushing the op itself saves a per-step alloc.
export const opCatchAll = function (handler) {
  return function (op) {
    return new Op(CATCH, K_CATCH, op, handler);
  };
};

export const opLocal = function (f) {
  return function (op) {
    return new Op(LOCAL, -1, f, op);
  };
};

// `register :: (a -> Effect Unit) -> (Variant e -> Effect Unit) -> Effect (Effect Unit)`
// The fiber calls `register(onOk)(onFail)()` and gets back an
// `Effect Unit` canceller. `onOk` and `onFail` are curried PureScript
// functions; the fiber wraps the underlying single-shot resume.
export const opAsync = function (register) {
  return new Op(ASYNC, -1, register, null);
};

export const opFork = function (op) {
  return new Op(FORK, -1, op, null);
};

// Like opFork but steps the child synchronously before returning the
// handle to the parent. If the child's body is fully sync, the child
// completes inline and the parent's subsequent `join` resolves without
// touching the microtask scheduler. If the child suspends, the parent
// gets a live handle exactly as with opFork.
export const opForkInline = function (op) {
  return new Op(FORK_INLINE, -1, op, null);
};

export const opJoin = function (fiber) {
  return new Op(JOIN, -1, fiber, null);
};

// Specialized array fork: take N ops and produce N fiber handles in
// one step. Equivalent to `traverse fork ops` but skips the per-element
// BIND chain (which `traverseArrayImpl` builds as a balanced ~2N-node
// tree). The handler walks the array in a tight JS loop.
export const opForkAll = function (ops) {
  return new Op(FORK_ALL, -1, ops, null);
};

// Specialized array join: take N fiber handles and produce their N
// results in order. Suspends until all complete; on the first non-OK
// outcome the parent propagates that outcome (sibling fibers continue
// running unmolested, matching ZIO's `Fiber.joinAll` semantics).
export const opJoinAll = function (fibers) {
  return new Op(JOIN_ALL, -1, fibers, null);
};

// Sequential traverse: run `fn(items[i])` for each i in order and
// collect the results. Skips the ~2N-node bind chain that
// `traverseArrayImpl` builds; instead the step loop holds a single
// K_FOR_EACH frame and advances `i` in place across resumptions.
export const opForEach = function (fn) {
  return function (items) {
    return new Op(FOR_EACH, -1, fn, items);
  };
};

// Dedicated map / apply ops. The Functor / Apply instances would
// otherwise express `map` as `bind m (\a -> pure (f a))` and `apply mf
// ma` as `bind mf (\f -> bind ma (\a -> pure (f a)))`. Each of those
// allocates BIND + PURE + closure objects on every node, and `traverse`
// composes them into a balanced tree, so the per-element cost was
// dominated by allocation churn. MAP / APPLY carry the function value
// directly so the runtime can fold it into the result without a PURE
// round-trip.
//
// The smart constructors below also fuse at construction time, so a
// 1000-deep `map` chain becomes one MAP node holding the composed
// function rather than 1000 nested MAP ops. The savings show up
// before the interpreter even gets a look at the tree, which matches
// what `Aff` does in its `Map` / `Bind` smart constructors. Fusions:
//
//   * map f (Pure a)    => Pure (f a)
//   * map f (Sync io)   => Sync (\_ -> f (io()))
//   * map f (Map g x)   => Map (f . g) x
//   * apply (Pure f) ma => map f ma            (then re-fuse)
//   * apply mf (Pure a) => map (\f -> f a) mf  (then re-fuse)
// Hot path is small enough to fit V8's inlining budget: a single tag
// check + a Pure / Map node alloc. The SYNC / MAP fusions live in
// opMapSlow so the curried call site can stay inlineable. The Pure
// fusion path here is the one a `map f (pure x)` chain hits every
// iteration; folding it inline collapses the chain to a single Pure
// at construction time, with no runtime dispatch involved.
export const opMap = function (f) {
  return function (op) {
    const tag = op._tag;
    if (tag === PURE) {
      return new Op(PURE, -1, f(op._1), null);
    }
    return opMapSlow(f, op, tag);
  };
};

function opMapSlow(f, op, tag) {
  if (tag === SYNC) {
    const run = op._1;
    return new Op(SYNC, -1, function () {
      return f(run());
    }, null);
  }
  if (tag === MAP) {
    const g = op._1;
    // _k = K_MAP so the fused MAP op doubles as its own K_MAP frame
    // (same trick as opApplySlow / opBind).
    return new Op(MAP, K_MAP, function (x) {
      return f(g(x));
    }, op._2);
  }
  return new Op(MAP, K_MAP, f, op);
}

// Same inlining trick as opMap. The PURE/PURE leaf (which is what a
// `pure f <*> pure a` chain hits every iteration) is in the hot path;
// the degenerate-to-map cases live in opApplySlow.
export const opApply = function (opF) {
  return function (opA) {
    if (opF._tag === PURE && opA._tag === PURE) {
      return new Op(PURE, -1, opF._1(opA._1), null);
    }
    return opApplySlow(opF, opA);
  };
};

function opApplySlow(opF, opA) {
  const tagF = opF._tag;
  const tagA = opA._tag;
  // Function arg is pure: degenerate to a map. Then re-run the map
  // fusions on top of opA, so e.g. `pure f <*> pure g <*> ma`
  // collapses through both sides.
  if (tagF === PURE) {
    return opMap(opF._1)(opA);
  }
  // Value arg is pure: degenerate to a map over the function arg.
  if (tagA === PURE) {
    const a = opA._1;
    return opMap(function (g) {
      return g(a);
    })(opF);
  }
  // _k = K_APPLY so the APPLY op doubles as its own K_APPLY frame:
  // the spine-walk push sites stack the op itself instead of allocating
  // a fresh `new Op(-1, K_APPLY, opA, null)` per level. K_APPLY's unwind
  // reads `frame._2` (opA), which is the same slot the APPLY op already
  // uses, so the dual-purpose layout matches without rearrangement.
  return new Op(APPLY, K_APPLY, opF, opA);
}

// Like opForkAll but each child is stepped synchronously before its
// handle lands in the result array, mirroring opForkInline for the
// batch case. PURE / SYNC leaves collapse to DoneFibers without
// touching the scheduler; everything else allocates a Fiber and is
// driven once inline so its first ASYNC callback is registered before
// the parent makes any further observable progress.
export const opForkAllInline = function (ops) {
  return new Op(FORK_ALL_INLINE, -1, ops, null);
};

export const opInterrupt = function (fiber) {
  return new Op(INTERRUPT, -1, fiber, null);
};

// ENSURING op doubles as its own K_ENSURE frame: `finalizer` is in
// _2, the unwind switch only reads `_k` and `_2`.
export const opEnsuring = function (finalizer) {
  return function (action) {
    return new Op(ENSURING, K_ENSURE, action, finalizer);
  };
};

export const opUninterruptible = function (op) {
  return new Op(UNINTERRUPTIBLE, -1, op, null);
};

export const opRace = function (left) {
  return function (right) {
    return new Op(RACE, -1, left, right);
  };
};

export const opParTraverse = function (fn) {
  return function (items) {
    return new Op(PAR_TRAVERSE, -1, fn, items);
  };
};

export const opPeel = function (op) {
  return new Op(PEEL, -1, op, null);
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
  return new Op(FREF_GET, -1, ref, null);
};

export const opSetFiberRef = function (ref) {
  return function (value) {
    return new Op(FREF_SET, -1, ref, value);
  };
};

export const opModifyFiberRef = function (ref) {
  return function (fn) {
    return new Op(FREF_MODIFY, -1, ref, fn);
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
  // Depth-tracked wrapper around _stepInner: only the outermost step()
  // call flushes the pending queue (sub-step()s nested via inline-drain
  // / interrupt / _resumeAsync just bump the counter).
  _inStep++;
  try {
    this._stepInner();
  } finally {
    _inStep--;
    if (_inStep === 0) _flushPending();
  }
};

Fiber.prototype._stepInner = function () {
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
          this.value = op._1;
          this.mode = M_OK;
          break;
        case SYNC:
          try {
            this.value = op._1();
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
            const inner = bindOp._1;
            switch (inner._tag) {
              case PURE: {
                const next = bindOp._2(inner._1);
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
                const body = inner._1;
                if (body._tag === PURE && _supervisors.length === 0) {
                  const next = bindOp._2(new DoneFiber(M_OK, body._1));
                  if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                  this.current = next;
                  break bindLoop;
                }
                this.frefsOwn = false;
                const child = new Fiber(body, this.env, this.frefs);
                scheduleFiber(child);
                const next = bindOp._2(child);
                if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                this.current = next;
                break bindLoop;
              }
              case JOIN: {
                const target = inner._1;
                if (target.queued) {
                  target.queued = false;
                  _pendingCount--;
                  target.step();
                }
                if (target.status === F_DONE) {
                  if (target.mode === M_OK) {
                    const next = bindOp._2(target.value);
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
                  v = inner._1();
                } catch (err) {
                  this.value = err;
                  this.mode = M_DIE;
                  break bindLoop;
                }
                const next = bindOp._2(v);
                if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                this.current = next;
                break bindLoop;
              }
              case ASK: {
                const next = bindOp._2(this.env);
                if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                this.current = next;
                break bindLoop;
              }
              case FORK_INLINE: {
                const body = inner._1;
                const bodyTag = body._tag;
                if (_supervisors.length === 0) {
                  if (bodyTag === PURE) {
                    const next = bindOp._2(new DoneFiber(M_OK, body._1));
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
                      v = body._1();
                      m = M_OK;
                    } catch (err) {
                      v = err;
                      m = M_DIE;
                    }
                    const next = bindOp._2(new DoneFiber(m, v));
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
                const next = bindOp._2(inlineChild);
                if (next._tag === BIND) { bindOp = next; continue bindLoop; }
                this.current = next;
                break bindLoop;
              }
              case FREF_GET: {
                const ref = inner._1;
                const m = this.frefs;
                const next = bindOp._2(
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
                const ops = inner._1;
                const n = ops.length;
                const out = new Array(n);
                if (n > 0) {
                  this.frefsOwn = false;
                  const supEmpty = _supervisors.length === 0;
                  for (let i = 0; i < n; i++) {
                    const body = ops[i];
                    if (body._tag === PURE && supEmpty) {
                      out[i] = new DoneFiber(M_OK, body._1);
                    } else {
                      const child = new Fiber(body, this.env, this.frefs);
                      scheduleFiber(child);
                      out[i] = child;
                    }
                  }
                }
                const next = bindOp._2(out);
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
                const fibers = inner._1;
                const n = fibers.length;
                if (n === 0) {
                  const next = bindOp._2([]);
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
                    _pendingCount--;
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
                  const next = bindOp._2(results);
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
              case APPLY:
              case MAP: {
                // Inline the APPLY / MAP spine walk so the typical
                // `do { xs <- traverse f arr; ... }` shape doesn't bounce
                // through the outer-loop tick / interrupt / switch trio
                // just to reach the apply tree. Push bindOp as its own
                // K_BIND frame at the bottom, then stack each APPLY op
                // (it doubles as its own K_APPLY frame) and one K_MAP
                // per MAP while descending into the spine. The leaf
                // becomes this.current and the unwind path folds back
                // up through K_MAP / K_APPLY and finally K_BIND.
                this.stack.push(bindOp);
                let cur = inner;
                while (true) {
                  const tag = cur._tag;
                  if (tag === APPLY) {
                    const lhs = cur._1;
                    const rhs = cur._2;
                    if (lhs._tag === PURE && rhs._tag === PURE) {
                      try {
                        this.value = lhs._1(rhs._1);
                        this.mode = M_OK;
                      } catch (err) {
                        this.value = err;
                        this.mode = M_DIE;
                      }
                      break;
                    }
                    this.stack.push(cur);
                    cur = lhs;
                    continue;
                  }
                  if (tag === MAP) {
                    this.stack.push(cur);
                    cur = cur._2;
                    continue;
                  }
                  this.current = cur;
                  break;
                }
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
                this.value = cur._1;
                this.mode = M_OK;
                this.current = null;
              } else if (curTag === SYNC) {
                this.current = null;
                try {
                  this.value = cur._1();
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
          this.value = op._1;
          this.mode = M_FAIL;
          break;
        case CATCH:
          // Reuse the op as its own K_CATCH frame; see opCatchAll.
          this.stack.push(op);
          this.current = op._1;
          continue;
        case ASK:
          this.value = this.env;
          this.mode = M_OK;
          break;
        case LOCAL: {
          const prev = this.env;
          this.env = op._1(this.env);
          this.stack.push(new Op(-1, K_LOCAL, prev, null));
          this.current = op._2;
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
            canceller = op._1(onOk)(onFail)();
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
          const body = op._1;
          if (body._tag === PURE && _supervisors.length === 0) {
            this.value = new DoneFiber(M_OK, body._1);
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
          const body = op._1;
          const bodyTag = body._tag;
          if (_supervisors.length === 0) {
            if (bodyTag === PURE) {
              this.value = new DoneFiber(M_OK, body._1);
              this.mode = M_OK;
              break;
            }
            if (bodyTag === SYNC) {
              let v;
              let m;
              try {
                v = body._1();
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
          const target = op._1;
          const self = this;
          if (target.queued) {
            target.queued = false;
            _pendingCount--;
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
          op._1.interrupt();
          this.value = undefined;
          this.mode = M_OK;
          break;
        }
        case ENSURING:
          // Reuse the op as its own K_ENSURE frame; see opEnsuring.
          this.stack.push(op);
          this.current = op._1;
          continue;
        case UNINTERRUPTIBLE:
          this.mask++;
          this.stack.push(FRAME_UNMASK);
          this.current = op._1;
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
          const leftFiber = new Fiber(op._1, this.env, this.frefs);
          const rightFiber = new Fiber(op._2, this.env, this.frefs);
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
          this.current = op._1;
          continue;
        case PAR_TRAVERSE: {
          const self = this;
          const fn = op._1;
          const items = op._2;
          const n = items.length;
          if (n === 0) {
            this.value = [];
            this.mode = M_OK;
            break;
          }
          // Fast path: precompute the bodies in one pass. If every
          // body is a PURE leaf, fold their values into the result
          // array without allocating fibers, observers, schedule
          // entries, or a canceller. The semantics are unchanged
          // (PURE bodies have no observable interleaving).
          const bodies = new Array(n);
          let allPure = true;
          for (let i = 0; i < n; i++) {
            const body = fn(items[i]);
            bodies[i] = body;
            if (body._tag !== PURE) allPure = false;
          }
          if (allPure && _supervisors.length === 0) {
            const results = new Array(n);
            for (let i = 0; i < n; i++) results[i] = bodies[i]._1;
            this.value = results;
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
            const child = new Fiber(bodies[idx], this.env, this.frefs);
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
          if (op._1._c === C_EMPTY) {
            this.value = undefined;
            this.mode = M_OK;
          } else {
            this.value = op._1;
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
          const ops = op._1;
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
              out[i] = new DoneFiber(M_OK, body._1);
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
        case MAP: {
          // Inline common leaf shapes so leaf-of-traverse patterns like
          // `map f (fork m)` / `map f (join fib)` / `map f (pure x)` /
          // `map f (liftEffect e)` don't pay a K_MAP frame and a
          // separate outer-dispatch tick on the inner op. Anything that
          // returns synchronously gets folded in place; only genuinely
          // suspending inner ops (e.g. JOIN on an unfinished fiber)
          // install a K_MAP frame and yield to the outer dispatch.
          const inner = op._2;
          const innerTag = inner._tag;
          const f = op._1;
          if (innerTag === PURE) {
            try {
              this.value = f(inner._1);
              this.mode = M_OK;
            } catch (err) {
              this.value = err;
              this.mode = M_DIE;
            }
            break;
          }
          if (innerTag === SYNC) {
            try {
              this.value = f(inner._1());
              this.mode = M_OK;
            } catch (err) {
              this.value = err;
              this.mode = M_DIE;
            }
            break;
          }
          if (innerTag === FORK) {
            const body = inner._1;
            let fiber;
            if (body._tag === PURE && _supervisors.length === 0) {
              fiber = new DoneFiber(M_OK, body._1);
            } else {
              this.frefsOwn = false;
              fiber = new Fiber(body, this.env, this.frefs);
              scheduleFiber(fiber);
            }
            try {
              this.value = f(fiber);
              this.mode = M_OK;
            } catch (err) {
              this.value = err;
              this.mode = M_DIE;
            }
            break;
          }
          if (innerTag === FORK_INLINE) {
            const body = inner._1;
            const bodyTag = body._tag;
            let fiber;
            if (_supervisors.length === 0 && bodyTag === PURE) {
              fiber = new DoneFiber(M_OK, body._1);
            } else if (_supervisors.length === 0 && bodyTag === SYNC) {
              let v;
              let m;
              try {
                v = body._1();
                m = M_OK;
              } catch (err) {
                v = err;
                m = M_DIE;
              }
              fiber = new DoneFiber(m, v);
            } else {
              this.frefsOwn = false;
              fiber = new Fiber(body, this.env, this.frefs);
              fiber.step();
            }
            try {
              this.value = f(fiber);
              this.mode = M_OK;
            } catch (err) {
              this.value = err;
              this.mode = M_DIE;
            }
            break;
          }
          if (innerTag === JOIN) {
            const target = inner._1;
            if (target.queued) {
              target.queued = false;
              _pendingCount--;
              target.step();
            }
            if (target.status === F_DONE) {
              if (target.mode === M_OK) {
                try {
                  this.value = f(target.value);
                  this.mode = M_OK;
                } catch (err) {
                  this.value = err;
                  this.mode = M_DIE;
                }
              } else {
                // Non-OK completed join: propagate result; skip f.
                this._installResult(target);
              }
              break;
            }
            // Suspending join: push the MAP op (doubles as K_MAP frame)
            // and dispatch JOIN.
            this.stack.push(op);
            this.current = inner;
            continue;
          }
          if (innerTag === ASK) {
            try {
              this.value = f(this.env);
              this.mode = M_OK;
            } catch (err) {
              this.value = err;
              this.mode = M_DIE;
            }
            break;
          }
          if (innerTag === FREF_GET) {
            const ref = inner._1;
            const m = this.frefs;
            const v = (m !== null && m.has(ref)) ? m.get(ref) : ref.initial;
            try {
              this.value = f(v);
              this.mode = M_OK;
            } catch (err) {
              this.value = err;
              this.mode = M_DIE;
            }
            break;
          }
          // Fallback: push the MAP op (doubles as K_MAP frame, _k=K_MAP).
          this.stack.push(op);
          this.current = inner;
          continue;
        }
        case APPLY: {
          // Evaluate opF first; K_APPLY then captures the resulting
          // function and kicks off opA. K_APPLY2 finally calls f(v).
          // Folds the PURE / PURE leaf to skip both frames entirely.
          //
          // Spine collapse: traverseArrayImpl builds a balanced tree of
          // `apply(apply(... ) y) z`, with MAP nodes carrying the
          // result-shape function at each level (`map array2`,
          // `map array3`, `map concat2`). Walk the left spine here,
          // pushing the APPLY op itself as a K_APPLY frame (it already
          // has _k=K_APPLY and carries opA in _2) and one K_MAP per
          // MAP, so a depth-N spine pays one outer-loop dispatch
          // instead of N. The walk stops at the first non-APPLY non-MAP
          // leaf (PURE, SYNC, FORK, JOIN, ASK, ...); the outer loop
          // dispatches that leaf and the unwind path folds the stacked
          // frames.
          let cur = op;
          while (true) {
            const tag = cur._tag;
            if (tag === APPLY) {
              const lhs = cur._1;
              const rhs = cur._2;
              if (lhs._tag === PURE && rhs._tag === PURE) {
                try {
                  this.value = lhs._1(rhs._1);
                  this.mode = M_OK;
                } catch (err) {
                  this.value = err;
                  this.mode = M_DIE;
                }
                break;
              }
              this.stack.push(cur);
              cur = lhs;
              continue;
            }
            if (tag === MAP) {
              this.stack.push(cur);
              cur = cur._2;
              continue;
            }
            this.current = cur;
            break;
          }
          if (this.current !== null) continue;
          break;
        }
        case FORK_ALL_INLINE: {
          // Batch variant of FORK_INLINE: spawn N fibers, drive each
          // one synchronously once before returning the handle array.
          // PURE / SYNC bodies collapse to DoneFibers (no Fiber alloc,
          // no scheduleFiber); everything else gets one inline step so
          // a subsequent JOIN_ALL can rendezvous on completed children
          // without the queueMicrotask round-trip.
          const ops = op._1;
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
            const bodyTag = body._tag;
            if (supEmpty && bodyTag === PURE) {
              out[i] = new DoneFiber(M_OK, body._1);
            } else if (supEmpty && bodyTag === SYNC) {
              let v;
              let m;
              try {
                v = body._1();
                m = M_OK;
              } catch (err) {
                v = err;
                m = M_DIE;
              }
              out[i] = new DoneFiber(m, v);
            } else {
              const child = new Fiber(body, this.env, this.frefs);
              child.step();
              out[i] = child;
            }
          }
          this.value = out;
          this.mode = M_OK;
          break;
        }
        case FOR_EACH: {
          // Sequential traverse. Mutates `i` and `results` in place
          // across iterations / resumptions so we pay one frame alloc
          // + one results-array alloc per forEach.
          //
          // Tight loop: as long as fn(items[i]) returns a PURE / SYNC
          // leaf we fold the result and advance i without leaving this
          // case. The frame only goes on the stack when the body is
          // genuinely suspending (or branches into a non-trivial op).
          // For pure / sync bodies the whole array runs in one outer
          // dispatch with no per-element frame push / pop.
          const items = op._2;
          const n = items.length;
          if (n === 0) {
            this.value = [];
            this.mode = M_OK;
            break;
          }
          const fn = op._1;
          const results = new Array(n);
          let i = 0;
          let bodyOp;
          forEachFast: while (true) {
            try {
              bodyOp = fn(items[i]);
            } catch (err) {
              this.value = err;
              this.mode = M_DIE;
              break;
            }
            const btag = bodyOp._tag;
            if (btag === PURE) {
              results[i] = bodyOp._1;
              i++;
              if (i >= n) {
                this.value = results;
                this.mode = M_OK;
                break;
              }
              continue forEachFast;
            }
            if (btag === SYNC) {
              try {
                results[i] = bodyOp._1();
              } catch (err) {
                this.value = err;
                this.mode = M_DIE;
                break forEachFast;
              }
              i++;
              if (i >= n) {
                this.value = results;
                this.mode = M_OK;
                break;
              }
              continue forEachFast;
            }
            // Suspending / structured body: install the frame and let
            // the outer loop dispatch it.
            const fr = new ForEachFrame(fn, items, results);
            fr.i = i;
            this.stack.push(fr);
            this.current = bodyOp;
            break;
          }
          continue;
        }
        case JOIN_ALL: {
          // Wait on a batch of pre-forked fibers. Synchronously drive
          // any that are still in the run queue (matches case JOIN's
          // queue-rendezvous fast path), collect successes in place,
          // and propagate the first non-OK outcome we see. If anything
          // is still pending after the sync pass, suspend and register
          // observers for the rest.
          const fibers = op._1;
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
              _pendingCount--;
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
          const ref = op._1;
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
          this.frefs.set(op._1, op._2);
          this.value = undefined;
          this.mode = M_OK;
          break;
        }
        case FREF_MODIFY: {
          const ref = op._1;
          const m0 = this.frefs;
          const prev = (m0 !== null && m0.has(ref)) ? m0.get(ref) : ref.initial;
          let next;
          try {
            next = op._2(prev);
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
            const nextOp = frame._2(this.value);
            const nextTag = nextOp._tag;
            if (nextTag === PURE) {
              this.value = nextOp._1;
              // mode is already M_OK
            } else if (nextTag === SYNC) {
              try {
                this.value = nextOp._1();
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
                const inner = bindOp._1;
                switch (inner._tag) {
                  case PURE: {
                    const next = bindOp._2(inner._1);
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
                    const body = inner._1;
                    if (body._tag === PURE && _supervisors.length === 0) {
                      const next = bindOp._2(new DoneFiber(M_OK, body._1));
                      if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                      this.current = next;
                      break kbindLoop;
                    }
                    this.frefsOwn = false;
                    const child = new Fiber(body, this.env, this.frefs);
                    scheduleFiber(child);
                    const next = bindOp._2(child);
                    if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                    this.current = next;
                    break kbindLoop;
                  }
                  case JOIN: {
                    const target = inner._1;
                    if (target.queued) {
                      target.queued = false;
                      _pendingCount--;
                      target.step();
                    }
                    if (target.status === F_DONE) {
                      if (target.mode === M_OK) {
                        const next = bindOp._2(target.value);
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
                      v = inner._1();
                    } catch (err) {
                      this.value = err;
                      this.mode = M_DIE;
                      break kbindLoop;
                    }
                    const next = bindOp._2(v);
                    if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                    this.current = next;
                    break kbindLoop;
                  }
                  case ASK: {
                    const next = bindOp._2(this.env);
                    if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                    this.current = next;
                    break kbindLoop;
                  }
                  case FORK_INLINE: {
                    const body = inner._1;
                    const bodyTag = body._tag;
                    if (_supervisors.length === 0) {
                      if (bodyTag === PURE) {
                        const next = bindOp._2(new DoneFiber(M_OK, body._1));
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
                          v = body._1();
                          m = M_OK;
                        } catch (err) {
                          v = err;
                          m = M_DIE;
                        }
                        const next = bindOp._2(new DoneFiber(m, v));
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
                    const next = bindOp._2(inlineChild);
                    if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                    this.current = next;
                    break kbindLoop;
                  }
                  case FORK_ALL: {
                    const ops = inner._1;
                    const n = ops.length;
                    const out = new Array(n);
                    if (n > 0) {
                      this.frefsOwn = false;
                      const supEmpty = _supervisors.length === 0;
                      for (let i = 0; i < n; i++) {
                        const body = ops[i];
                        if (body._tag === PURE && supEmpty) {
                          out[i] = new DoneFiber(M_OK, body._1);
                        } else {
                          const child = new Fiber(body, this.env, this.frefs);
                          scheduleFiber(child);
                          out[i] = child;
                        }
                      }
                    }
                    const next = bindOp._2(out);
                    if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                    this.current = next;
                    break kbindLoop;
                  }
                  case JOIN_ALL: {
                    const fibers = inner._1;
                    const n = fibers.length;
                    if (n === 0) {
                      const next = bindOp._2([]);
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
                        _pendingCount--;
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
                      const next = bindOp._2(results);
                      if (next._tag === BIND) { bindOp = next; continue kbindLoop; }
                      this.current = next;
                      break kbindLoop;
                    }
                    this.stack.push(bindOp);
                    this.current = inner;
                    break kbindLoop;
                  }
                  case APPLY:
                  case MAP: {
                    // Mirror the bindLoop fast path: walk the APPLY / MAP
                    // spine with bindOp already pushed as the bottom
                    // K_BIND frame so the unwind chain is ready by the
                    // time we dispatch the leaf. Each APPLY op is its
                    // own K_APPLY frame (set in opApplySlow).
                    this.stack.push(bindOp);
                    let cur = inner;
                    while (true) {
                      const tag = cur._tag;
                      if (tag === APPLY) {
                        const lhs = cur._1;
                        const rhs = cur._2;
                        if (lhs._tag === PURE && rhs._tag === PURE) {
                          try {
                            this.value = lhs._1(rhs._1);
                            this.mode = M_OK;
                          } catch (err) {
                            this.value = err;
                            this.mode = M_DIE;
                          }
                          break;
                        }
                        this.stack.push(cur);
                        cur = lhs;
                        continue;
                      }
                      if (tag === MAP) {
                        this.stack.push(cur);
                        cur = cur._2;
                        continue;
                      }
                      this.current = cur;
                      break;
                    }
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
                    this.value = cur._1;
                    this.current = null;
                    // mode is already M_OK
                  } else if (curTag === SYNC) {
                    this.current = null;
                    try {
                      this.value = cur._1();
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
            this.current = frame._2(this.value);
            this.mode = M_OK;
            this.value = null;
          }
          // OK passes through; DIE / INTERRUPT bypass user handlers.
          break;
        case K_LOCAL:
          this.env = frame._1;
          break;
        case K_ENSURE: {
          // Save the action's outcome, enter uninterruptible region,
          // and arrange for the finalizer to run.
          this.mask++;
          this.stack.push(new Op(-1, K_AFTER_FIN, this.value, this.mode));
          this.current = frame._2;
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
          const savedValue = frame._1;
          const savedMode = frame._2;
          if (this.mode === M_OK && savedMode === M_OK) {
            this.value = savedValue;
            this.mode = M_OK;
          } else if (this.mode === M_OK) {
            // finalizer ok; restore action's failure exactly
            this.value = savedValue;
            this.mode = savedMode;
          } else if (savedMode === M_OK) {
            // action ok, finalizer failed; propagate finalizer
            // (mode/value already in place)
          } else {
            // both non-ok: compose
            const combined = causeThen(
              modeToCause(savedMode, savedValue),
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
            const results = frame.results;
            const items = frame.items;
            const n = frame.n;
            const fn = frame.fn;
            results[frame.i] = this.value;
            let i = frame.i + 1;
            // Tight unwind loop mirroring the FOR_EACH dispatch fast
            // path. Fold PURE / SYNC bodies inline so the frame stays
            // off the stack until something actually suspends.
            forEachUnwind: while (i < n) {
              let nextOp;
              try {
                nextOp = fn(items[i]);
              } catch (err) {
                this.value = err;
                this.mode = M_DIE;
                break forEachUnwind;
              }
              const ntag = nextOp._tag;
              if (ntag === PURE) {
                results[i] = nextOp._1;
                i++;
                continue forEachUnwind;
              }
              if (ntag === SYNC) {
                try {
                  results[i] = nextOp._1();
                } catch (err) {
                  this.value = err;
                  this.mode = M_DIE;
                  break forEachUnwind;
                }
                i++;
                continue forEachUnwind;
              }
              // Suspending body: reinstall the frame at the new i
              // and let the outer loop pick it up.
              frame.i = i;
              this.stack.push(frame);
              this.current = nextOp;
              this.value = null;
              break;
            }
            if (i >= n && this.mode === M_OK) {
              this.value = results;
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
        case K_MAP: {
          if (this.mode === M_OK) {
            try {
              this.value = frame._1(this.value);
            } catch (err) {
              this.value = err;
              this.mode = M_DIE;
            }
          }
          // FAIL / DIE / INTERRUPT / CAUSE: pass through.
          break;
        }
        case K_APPLY: {
          // opF just completed. On success, capture its value (the
          // function) and continue with opA. For simple leaf shapes of
          // opA we fold the result inline so the K_APPLY2 + outer
          // dispatch round-trip vanishes. Only genuinely suspending
          // opAs (e.g. JOIN on an unfinished fiber) take the slow path.
          //
          // `frame` here is the APPLY op itself (pushed by the spine
          // walks via _k=K_APPLY); opA lives in frame._2.
          if (this.mode === M_OK) {
            const f = this.value;
            const opA = frame._2;
            const opATag = opA._tag;
            if (opATag === PURE) {
              try {
                this.value = f(opA._1);
                this.mode = M_OK;
              } catch (err) {
                this.value = err;
                this.mode = M_DIE;
              }
              break;
            }
            if (opATag === SYNC) {
              try {
                this.value = f(opA._1());
                this.mode = M_OK;
              } catch (err) {
                this.value = err;
                this.mode = M_DIE;
              }
              break;
            }
            if (opATag === FORK) {
              const body = opA._1;
              let fiber;
              if (body._tag === PURE && _supervisors.length === 0) {
                fiber = new DoneFiber(M_OK, body._1);
              } else {
                this.frefsOwn = false;
                fiber = new Fiber(body, this.env, this.frefs);
                scheduleFiber(fiber);
              }
              try {
                this.value = f(fiber);
                this.mode = M_OK;
              } catch (err) {
                this.value = err;
                this.mode = M_DIE;
              }
              break;
            }
            if (opATag === FORK_INLINE) {
              const body = opA._1;
              const bodyTag = body._tag;
              let fiber;
              if (_supervisors.length === 0 && bodyTag === PURE) {
                fiber = new DoneFiber(M_OK, body._1);
              } else if (_supervisors.length === 0 && bodyTag === SYNC) {
                let v, m;
                try { v = body._1(); m = M_OK; }
                catch (err) { v = err; m = M_DIE; }
                fiber = new DoneFiber(m, v);
              } else {
                this.frefsOwn = false;
                fiber = new Fiber(body, this.env, this.frefs);
                fiber.step();
              }
              try {
                this.value = f(fiber);
                this.mode = M_OK;
              } catch (err) {
                this.value = err;
                this.mode = M_DIE;
              }
              break;
            }
            if (opATag === JOIN) {
              const target = opA._1;
              if (target.queued) {
                target.queued = false;
                _pendingCount--;
                target.step();
              }
              if (target.status === F_DONE) {
                if (target.mode === M_OK) {
                  try {
                    this.value = f(target.value);
                    this.mode = M_OK;
                  } catch (err) {
                    this.value = err;
                    this.mode = M_DIE;
                  }
                } else {
                  this._installResult(target);
                }
                break;
              }
              // Suspending join: K_APPLY2 + outer dispatch.
              this.stack.push(new Op(-1, K_APPLY2, f, null));
              this.current = opA;
              this.value = null;
              break;
            }
            if (opATag === ASK) {
              try {
                this.value = f(this.env);
                this.mode = M_OK;
              } catch (err) {
                this.value = err;
                this.mode = M_DIE;
              }
              break;
            }
            // Slow path: opA is an APPLY or MAP subtree (typical for
            // the outer arms of `traverseArrayImpl`'s balanced tree).
            // Drop K_APPLY2(f) as the bottom frame, then walk opA's
            // own APPLY / MAP spine inline so the subtree dispatches
            // its leaf with the unwind chain already in place. Saves
            // one outer-loop dispatch per K_APPLY-into-APPLY transition.
            if (opATag === APPLY || opATag === MAP) {
              this.stack.push(new Op(-1, K_APPLY2, f, null));
              let cur = opA;
              while (true) {
                const tag = cur._tag;
                if (tag === APPLY) {
                  const lhs = cur._1;
                  const rhs = cur._2;
                  if (lhs._tag === PURE && rhs._tag === PURE) {
                    try {
                      this.value = lhs._1(rhs._1);
                      this.mode = M_OK;
                    } catch (err) {
                      this.value = err;
                      this.mode = M_DIE;
                    }
                    break;
                  }
                  this.stack.push(cur);
                  cur = lhs;
                  continue;
                }
                if (tag === MAP) {
                  this.stack.push(cur);
                  cur = cur._2;
                  continue;
                }
                this.current = cur;
                this.value = null;
                break;
              }
              break;
            }
            this.stack.push(new Op(-1, K_APPLY2, f, null));
            this.current = opA;
            this.value = null;
          }
          break;
        }
        case K_APPLY2: {
          if (this.mode === M_OK) {
            try {
              this.value = frame._1(this.value);
            } catch (err) {
              this.value = err;
              this.mode = M_DIE;
            }
          }
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
//
// Lazy drain scheduling: a fiber scheduled while we're already inside a
// `step()` (depth counter `_inStep` > 0) does not pay queueMicrotask
// up-front. The outer step's wrapper checks `_pendingCount` on exit and
// schedules the drain only if any scheduled fiber survives the inline
// JOIN / JOIN_ALL drains. The fork-fan-out + join-fan-in pattern, the
// common case for parTraverse-style fan-outs, then pays zero
// queueMicrotask round-trips in the all-sync-children case.
const _runQueue = [];
let _drainScheduled = false;
let _inStep = 0;
let _pendingCount = 0;

const _queueDrain =
  typeof queueMicrotask !== "undefined"
    ? function (cb) { queueMicrotask(cb); }
    : function (cb) { setTimeout(cb, 0); };

function _runDrain() {
  _drainScheduled = false;
  // Bump _inStep across the whole drain so scheduleFibers fired by
  // the steps we run don't trigger a fresh queueMicrotask; the index
  // walk picks them up before we clear the queue.
  _inStep++;
  try {
    let i = 0;
    while (i < _runQueue.length) {
      const f = _runQueue[i];
      i++;
      if (f.queued) {
        f.queued = false;
        _pendingCount--;
        f.step();
      }
      // else: JOIN drove it inline; the inline-drainer decremented
      // _pendingCount when it set queued=false.
    }
    _runQueue.length = 0;
  } finally {
    _inStep--;
  }
}

function scheduleFiber(f) {
  f.queued = true;
  _runQueue.push(f);
  _pendingCount++;
  // Inside another step (this one, an inline-drain, or a drain pass)
  // we defer queueMicrotask: the outer call will _flushPending on exit.
  if (_inStep > 0) return;
  if (!_drainScheduled) {
    _drainScheduled = true;
    _queueDrain(_runDrain);
  }
}

function _flushPending() {
  // Called at the outermost step() exit. If anything is still queued
  // (i.e. wasn't picked up by an inline JOIN / JOIN_ALL drain), drain
  // it inline on this call stack. That's both faster than scheduling
  // a fresh queueMicrotask (no scheduler round-trip) and preserves the
  // ordering invariant non-trivial tests rely on: a fiber that was
  // `fork`ed before a synchronous operation must have registered its
  // ASYNC callbacks by the time that operation runs. Lazy-deferring
  // the drain to a microtask would let later synchronous work (e.g.
  // `TestClock.advance`) observe a pre-`fork` world.
  //
  // Re-entry: _runDrain bumps `_inStep` itself, so f.step() calls it
  // makes won't trigger another _flushPending. New scheduleFibers
  // appended during the drain are picked up by the same index walk.
  if (_pendingCount === 0) return;
  _runDrain();
}

// Top-level entry points -----------------------------------------------

export const _startFiber = function (op) {
  return function (env) {
    return function () {
      // Fast path: a top-level PURE / SYNC op has no need for a Fiber.
      // Both `runRIO'` on a smart-constructor-fused chain (`map (_ + 1)`
      // over `pure 0`, `pure f <*> pure a`, ...) and `runRIO'` on a bare
      // `liftEffect e` land here after construction, so handing back a
      // DoneFiber stub directly skips the new Fiber + step() + unwind +
      // resultToOutcome round-trip per call. Guarded on `_supervisors`
      // so onStart / onEnd hooks still see every fiber when they're
      // registered.
      if (_supervisors.length === 0) {
        const tag = op._tag;
        if (tag === PURE) {
          return new DoneFiber(M_OK, op._1);
        }
        if (tag === SYNC) {
          try {
            return new DoneFiber(M_OK, op._1());
          } catch (err) {
            return new DoneFiber(M_DIE, err);
          }
        }
      }
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

// Fused synchronous runner. The PS wrapper layer would otherwise call
// startFiber, then _fiberIsDone, then _fiberResult, then
// resultToOutcome (which itself allocs a Success / Die / Fail and
// dispatches on three mode predicates), then wrap the Outcome in a
// Maybe, then in runRIO pattern-match the Maybe-of-Outcome and alloc
// an Either, then in runRIO' pattern-match the Either and unwrap.
// That's five constructor allocations and several FFI predicate calls
// per runRIO' invocation, all to compute "did the fiber finish with
// OK". Inline the whole pipeline here so the OK path is one tag check
// + one field read, and the failure paths throw directly.
//
// The type at the PS boundary is `Op r e a -> Record r -> Effect a`;
// callers that statically know `e = ()` (i.e. runRIO') use it raw, and
// callers that need the typed-failure path (runRIO) keep going
// through runFiberSync.
export const _runFiberSyncOrThrow = function (op) {
  return function (env) {
    return function () {
      if (_supervisors.length === 0) {
        const tag = op._tag;
        if (tag === PURE) {
          return op._1;
        }
        if (tag === SYNC) {
          return op._1();
        }
      }
      const f = new Fiber(op, env);
      f.step();
      if (f.status !== F_DONE) {
        throw new Error("rio-fiber: program suspended; use runRIOCallback");
      }
      const mode = f.mode;
      if (mode === M_OK) {
        return f.value;
      }
      if (mode === M_DIE) {
        throw f.value;
      }
      if (mode === M_INTERRUPT) {
        throw new Error("rio-fiber: program was interrupted");
      }
      if (mode === M_CAUSE) {
        const leaf = _causeRepresentative(f.value);
        if (leaf === null) {
          throw new Error("rio-fiber: program was interrupted");
        }
        throw leaf;
      }
      // M_FAIL: at type `RIO () () a` this is statically unreachable
      // (the error row is uninhabited), but defend against misuse by
      // throwing a JS error rather than silently returning a Variant.
      throw new Error("rio-fiber: typed failure escaped runRIO'");
    };
  };
};

// Fused synchronous runner for `runRIO`. Same idea as
// `_runFiberSyncOrThrow` but the typed-failure path becomes a `Left
// variant`, returned through the Either constructors the PS side
// already pattern-matches. Defects / interrupts / suspensions still
// throw (unchanged from the layered runner). Skips the resultToOutcome
// + Maybe wrap + Outer Outcome -> Either pattern match.
//
// Imports `Right` / `Left` from `Data.Either`'s compiled output, which
// is a tiny dependency on a long-stable encoding. The PS runRIO does
// `instanceof Right / instanceof Left` already, so we have to use the
// real constructors.
export const _runFiberSyncEither = function (op) {
  return function (env) {
    return function () {
      if (_supervisors.length === 0) {
        const tag = op._tag;
        if (tag === PURE) {
          return new _Right(op._1);
        }
        if (tag === SYNC) {
          return new _Right(op._1());
        }
      }
      const f = new Fiber(op, env);
      f.step();
      if (f.status !== F_DONE) {
        throw new Error("rio-fiber: program suspended; use runRIOCallback");
      }
      const mode = f.mode;
      if (mode === M_OK) {
        return new _Right(f.value);
      }
      if (mode === M_FAIL) {
        return new _Left(f.value);
      }
      if (mode === M_DIE) {
        throw f.value;
      }
      if (mode === M_INTERRUPT) {
        throw new Error("rio-fiber: program was interrupted");
      }
      // M_CAUSE: peel a representative leaf and either return it as
      // Left (Fail) or throw it (Die / Interrupt).
      const c = f.value;
      const cTag = c._c;
      if (cTag === 1) {
        return new _Left(c.fail);
      }
      const leaf = _causeRepresentative(c);
      if (leaf === null) {
        throw new Error("rio-fiber: program was interrupted");
      }
      throw leaf;
    };
  };
};

// Walk a Cause tree and return a representative leaf payload to throw.
// Prefers defects (Die) since they always carry an Error; falls back
// to Fail payloads (which are Variants and won't be useful directly,
// but at least propagate something) and finally null for an
// interrupt-only cause.
function _causeRepresentative(c) {
  const t = c._c;
  if (t === 2) {
    return c.die;
  }
  if (t === 1) {
    return c.fail;
  }
  if (t === 4 || t === 5) {
    const l = _causeRepresentative(c.left);
    if (l !== null) return l;
    return _causeRepresentative(c.right);
  }
  return null;
}

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
  return new Op(FAIL_CAUSE, -1, c, null);
};
