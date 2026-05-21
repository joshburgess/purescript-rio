"use strict";

// Obtain the async iterator from an AsyncIterable.
export const _getAsyncIterator = function (iterable) {
  return function () {
    return iterable[Symbol.asyncIterator]();
  };
};

// Pull the next value from an async iterator. The callback receives
// one of `onValue(a)()`, `onEnd()`, or `onError(err)()` depending on
// how the underlying `next()` resolves. A synchronous throw from
// `next()` is routed through `onError`.
export const _pullNext = function (iterator) {
  return function (onValue) {
    return function (onEnd) {
      return function (onError) {
        return function () {
          var p;
          try {
            p = iterator.next();
          } catch (e) {
            onError(e)();
            return;
          }
          Promise.resolve(p).then(
            function (r) {
              if (r && r.done) {
                onEnd();
              } else {
                onValue(r.value)();
              }
            },
            function (e) {
              onError(e)();
            }
          );
        };
      };
    };
  };
};

// Allocate the JS-side machinery that backs an AsyncIterable exposed
// to JS consumers. The handle exposes `push`, `end`, and `fail`
// operations that the PureScript producer fiber drives.
export const _mkAsyncIterableHandle = function () {
  var buffer = [];
  var waiters = [];
  var ended = false;
  var rejectionError = null;

  function push(v) {
    if (waiters.length > 0) {
      var w = waiters.shift();
      w.resolve({ value: v, done: false });
    } else {
      buffer.push(v);
    }
  }

  function end() {
    if (ended) return;
    ended = true;
    while (waiters.length > 0) {
      var w = waiters.shift();
      w.resolve({ value: undefined, done: true });
    }
  }

  function fail(err) {
    if (ended) return;
    ended = true;
    rejectionError = err;
    while (waiters.length > 0) {
      var w = waiters.shift();
      w.reject(err);
    }
  }

  var iterable = {};
  iterable[Symbol.asyncIterator] = function () {
    return {
      next: function () {
        if (buffer.length > 0) {
          return Promise.resolve({ value: buffer.shift(), done: false });
        }
        if (ended) {
          if (rejectionError !== null) {
            return Promise.reject(rejectionError);
          }
          return Promise.resolve({ value: undefined, done: true });
        }
        return new Promise(function (resolve, reject) {
          waiters.push({ resolve: resolve, reject: reject });
        });
      }
    };
  };

  return { iterable: iterable, push: push, end: end, fail: fail };
};

export const _handlePush = function (handle) {
  return function (v) {
    return function () {
      handle.push(v);
    };
  };
};

export const _handleEnd = function (handle) {
  return function () {
    handle.end();
  };
};

export const _handleFail = function (handle) {
  return function (err) {
    return function () {
      handle.fail(err);
    };
  };
};

export const _handleIterable = function (handle) {
  return handle.iterable;
};
