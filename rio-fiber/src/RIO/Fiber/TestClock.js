"use strict";

export const _queueMicrotask = function (eff) {
  return function () {
    if (typeof queueMicrotask !== "undefined") {
      queueMicrotask(function () {
        eff();
      });
    } else {
      setTimeout(function () {
        eff();
      }, 0);
    }
  };
};
