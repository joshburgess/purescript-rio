"use strict";

// Build an AsyncIterable that yields each item of `xs` in order.
export const _asyncIterableOf = function (xs) {
  return function () {
    var arr = xs.slice();
    var idx = 0;
    var iterable = {};
    iterable[Symbol.asyncIterator] = function () {
      return {
        next: function () {
          if (idx >= arr.length) {
            return Promise.resolve({ value: undefined, done: true });
          }
          var v = arr[idx];
          idx += 1;
          return Promise.resolve({ value: v, done: false });
        }
      };
    };
    return iterable;
  };
};

// Build an AsyncIterable that yields every item of `xs` then rejects
// the subsequent `next()` with `Error(msg)`.
export const _asyncIterableRejecting = function (xs) {
  return function (msg) {
    return function () {
      var arr = xs.slice();
      var idx = 0;
      var iterable = {};
      iterable[Symbol.asyncIterator] = function () {
        return {
          next: function () {
            if (idx >= arr.length) {
              return Promise.reject(new Error(msg));
            }
            var v = arr[idx];
            idx += 1;
            return Promise.resolve({ value: v, done: false });
          }
        };
      };
      return iterable;
    };
  };
};
