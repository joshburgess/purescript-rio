#!/usr/bin/env bash
# Compile-fail test driver.
#
# Each case under cases/ is a `.purs` file paired with a `.expected` file.
# For each case the driver:
#   1. Copies the case into scratch/src/Scratch.purs.
#   2. Runs `npx spago build -p rio-compile-fail-scratch`.
#   3. Asserts the build *failed* (non-zero exit) and that the build
#      output contains the substring in the matching `.expected` file.
#   4. Restores scratch/src/Scratch.purs to its placeholder state.
#
# Exits 0 if every case is correctly rejected and contains the expected
# error substring; non-zero otherwise.

set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
SCRATCH_FILE="$HERE/scratch/src/Scratch.purs"
PLACEHOLDER="$(cat <<'EOF'
-- This file is a scratch pad for the compile-fail test driver.
-- The driver script (compile-fail/run.sh) overwrites this with
-- one case at a time, builds it, asserts the build fails, and then
-- restores this placeholder.
--
-- If you find this file in any other state, run `compile-fail/run.sh`
-- to restore it.
module Scratch where
EOF
)"

restore_placeholder() {
  printf '%s\n' "$PLACEHOLDER" >"$SCRATCH_FILE"
}
trap restore_placeholder EXIT

cd "$ROOT"

pass_count=0
fail_count=0

for case_file in "$HERE"/cases/*.purs; do
  case_name="$(basename "$case_file" .purs)"
  expected_file="${case_file%.purs}.expected"

  if [ ! -f "$expected_file" ]; then
    echo "SKIP $case_name: missing $expected_file"
    fail_count=$((fail_count + 1))
    continue
  fi

  cp "$case_file" "$SCRATCH_FILE"
  output="$(npx spago build -p rio-compile-fail-scratch 2>&1)"
  exit_code=$?

  if [ "$exit_code" -eq 0 ]; then
    echo "FAIL $case_name: build succeeded (expected failure)"
    fail_count=$((fail_count + 1))
    continue
  fi

  expected="$(head -n 1 "$expected_file")"
  if echo "$output" | grep -q "$expected"; then
    echo "PASS $case_name (got $expected as expected)"
    pass_count=$((pass_count + 1))
  else
    echo "FAIL $case_name: build failed but did not contain '$expected'"
    echo "----- build output -----"
    echo "$output"
    echo "------------------------"
    fail_count=$((fail_count + 1))
  fi
done

echo ""
echo "compile-fail: $pass_count passed, $fail_count failed"
if [ "$fail_count" -gt 0 ]; then
  exit 1
fi
