"use strict";

// Deferred: a one-shot cell that fibers can await and any other fiber
// can complete. Internally a state flag, the captured result, and a
// Map<id, {onOk, onFail}> of pending waiters. Each await call gets a
// numeric id back; passing it to _unsubscribe removes the waiter so
// late completions don't fire into an interrupted fiber.

export const _make = function () {
  return { state: 0, result: null, waiters: new Map(), nextId: 0 };
};

function _complete(d, r) {
  if (d.state === 1) return false;
  d.state = 1;
  d.result = r;
  const ws = d.waiters;
  d.waiters = new Map();
  ws.forEach(function (cbs) {
    try {
      if (Object.prototype.hasOwnProperty.call(r, "ok")) {
        cbs.onOk(r.ok)();
      } else {
        cbs.onFail(r.fail)();
      }
    } catch (_) {
      // never let an observer throw stall the rest
    }
  });
  return true;
}

export const _await = function (d) {
  return function (onOk) {
    return function (onFail) {
      return function () {
        if (d.state === 1) {
          if (Object.prototype.hasOwnProperty.call(d.result, "ok")) {
            onOk(d.result.ok)();
          } else {
            onFail(d.result.fail)();
          }
          return -1;
        }
        const id = d.nextId++;
        d.waiters.set(id, { onOk: onOk, onFail: onFail });
        return id;
      };
    };
  };
};

export const _unsubscribe = function (d) {
  return function (id) {
    return function () {
      if (id >= 0) d.waiters.delete(id);
    };
  };
};

export const _succeed = function (d) {
  return function (a) {
    return function () {
      return _complete(d, { ok: a });
    };
  };
};

export const _fail = function (d) {
  return function (v) {
    return function () {
      return _complete(d, { fail: v });
    };
  };
};

export const _pollIsDone = function (d) {
  return function () {
    return d.state === 1;
  };
};

export const _pollIsOk = function (d) {
  return function () {
    return (
      d.result !== null &&
      Object.prototype.hasOwnProperty.call(d.result, "ok")
    );
  };
};

export const _pollOk = function (d) {
  return function () {
    return d.result.ok;
  };
};

export const _pollFail = function (d) {
  return function () {
    return d.result.fail;
  };
};
