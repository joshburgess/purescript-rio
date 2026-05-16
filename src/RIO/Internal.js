"use strict";

// Marker key attached to Error instances that carry an RIO typed failure.
// Anything stored under this key is treated as a Variant payload.
const RIO_TYPED_FAILURE = "__rio$typedFailure";

export const _mkTypedFailure = function (variant) {
  // `new Error` with a fixed message keeps stack traces lightweight while
  // still letting `instanceof Error` work for downstream code.
  const e = new Error("RIO typed failure");
  e[RIO_TYPED_FAILURE] = variant;
  return e;
};

export const _matchTypedFailure = function (nothing) {
  return function (just) {
    return function (err) {
      if (err !== null && typeof err === "object" && RIO_TYPED_FAILURE in err) {
        return just(err[RIO_TYPED_FAILURE]);
      } else {
        return nothing;
      }
    };
  };
};
