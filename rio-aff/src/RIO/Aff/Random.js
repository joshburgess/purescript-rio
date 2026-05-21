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
