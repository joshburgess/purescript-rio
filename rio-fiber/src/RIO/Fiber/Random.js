"use strict";

// Prefer the Web Crypto API (available on globalThis in modern Node
// and browsers). Fall back to Node's `crypto.webcrypto` so older Node
// runtimes still work.
const webCrypto =
  (typeof globalThis !== "undefined" && globalThis.crypto)
    ? globalThis.crypto
    : require("crypto").webcrypto;

export const _uuid = function () {
  return webCrypto.randomUUID();
};

export const _bytes = function (n) {
  return function () {
    const buf = new Uint8Array(n);
    webCrypto.getRandomValues(buf);
    const out = new Array(n);
    for (let i = 0; i < n; i++) out[i] = buf[i];
    return out;
  };
};

// In-place Fisher-Yates shuffle over a copy of `arr`. The int
// generator is the active `Random` service's `int` field, which is
// curried as `lo -> hi -> Effect Int` (each layer being a JS unary
// function; the trailing `()` runs the Effect).
export const _shuffleWith = function (intGen) {
  return function (arr) {
    return function () {
      const out = arr.slice();
      for (let i = out.length - 1; i > 0; i--) {
        const j = intGen(0)(i)();
        const tmp = out[i];
        out[i] = out[j];
        out[j] = tmp;
      }
      return out;
    };
  };
};
