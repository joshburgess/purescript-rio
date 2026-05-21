export const newAbortController = () => new AbortController();

export const signalOf = (controller) => controller.signal;

export const abort = (controller) => () => {
  controller.abort();
};

export const isAborted = (signal) => () => signal.aborted;
