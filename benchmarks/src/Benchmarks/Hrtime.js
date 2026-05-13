// Returns nanoseconds as Number (loses precision after ~104 days but
// fine for benchmark deltas).
export const hrtimeNs = function () {
  const hr = process.hrtime();
  return hr[0] * 1e9 + hr[1];
};
