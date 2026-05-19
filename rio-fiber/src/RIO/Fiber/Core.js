"use strict";

export const _setTimeout = function (ms) {
  return function (eff) {
    return function () {
      return setTimeout(function () {
        eff();
      }, ms);
    };
  };
};

export const _clearTimeout = function (id) {
  return function () {
    clearTimeout(id);
  };
};
