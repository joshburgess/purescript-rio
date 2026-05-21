"use strict";

// Run an Effect to obtain a Promise, then invoke `onResolved` /
// `onRejected` with the result. The Promise's resolution is the only
// way to leave this function; cancellation of the surrounding RIO
// fiber does not abort the underlying Promise (JS has no general
// cancellation), but the fiber moves on without waiting for it.
export const _runPromise = function (mk) {
  return function (onResolved) {
    return function (onRejected) {
      return function () {
        var p;
        try {
          p = mk();
        } catch (e) {
          onRejected(e)();
          return;
        }
        Promise.resolve(p).then(
          function (a) { onResolved(a)(); },
          function (e) { onRejected(e)(); }
        );
      };
    };
  };
};
