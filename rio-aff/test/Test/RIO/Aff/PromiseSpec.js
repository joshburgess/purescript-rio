"use strict";

export const _resolved = function (a) {
  return function () {
    return Promise.resolve(a);
  };
};

export const _rejected = function (msg) {
  return function () {
    return Promise.reject(new Error(msg));
  };
};

export const _throwing = function (msg) {
  return function () {
    throw new Error(msg);
  };
};
