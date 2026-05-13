"use strict";

export const exitImpl = function (code) {
  return function () {
    process.exit(code);
  };
};
