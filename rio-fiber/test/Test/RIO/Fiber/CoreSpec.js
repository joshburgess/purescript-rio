"use strict";

const schedule =
  typeof queueMicrotask !== "undefined"
    ? function (f) {
        queueMicrotask(f);
      }
    : function (f) {
        setTimeout(f, 0);
      };

export const scheduleResume = function (eff) {
  return function () {
    schedule(function () {
      eff();
    });
  };
};
