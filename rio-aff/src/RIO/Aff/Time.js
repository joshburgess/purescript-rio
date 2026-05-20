export const toISOStringImpl = (ms) => () => new Date(ms).toISOString();

export const parseISO8601Impl = (s) => Date.parse(s);
