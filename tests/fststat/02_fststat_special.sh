#!/usr/bin/env bash
# Test fststat on a file with "special" fields that trigger an RMN error.
#
# Currently this is a single 64-bit SIGNED field. fststat does not handle
# 64-bit data (see loop_fields.F90: "We don't deal with 8-byte elements
# yet"): the record is read with a 4-byte pointer, producing garbage values
# and an RMN error ("compact_u_integer: Can only (un)compact integers with
# size <= 32 bits"), which makes fststat exit non-zero.
#
# This test documents that behavior:
#   (a) fststat runs to completion (the final "Status" banner is printed; a
#       crash would omit it),
#   (b) fststat exits non-zero, and
#   (c) the record's output line is printed for inspection.
#
# If fststat is fixed to skip 64-bit records gracefully, this test must be
# updated (the exit code would become 0).
#
# Usage: 02_fststat_special.sh <fststat>

set -u

if [ $# -lt 1 ]; then
    echo "Need argument to specify the fststat executable"
    exit 1
fi
FSTSTAT="$1"

# Prefer the XDF file (works with both the 20.0 and 20.1 backends); fall back
# to the RSF file if only that one is present.
test_file_in=06_fststat_special_in.xdf
if [ ! -f "${test_file_in}" ] && [ -f 06_fststat_special_in.rsf ]; then
    test_file_in=06_fststat_special_in.rsf
fi
if [ ! -f "${test_file_in}" ]; then
    echo "Test FST file 06_fststat_special_in.{xdf,rsf} does not exist!"
    exit 1
fi

echo "FSTSTAT = ${FSTSTAT}"
echo "Input file = ${test_file_in}"

output=$("${FSTSTAT}" -fst "${test_file_in}" 2>&1)
rc=$?

pass=0
fail=0
ok()   { pass=$((pass + 1)); echo "PASS: $1"; }
bad()  { fail=$((fail + 1)); echo "FAIL: $1"; }

echo "=== (a) ran to completion ==="
if echo "${output}" | grep -qE '^Status[[:space:]]*:'; then
    ok "final Status banner present"
else
    bad "final Status banner missing (did fststat crash?)"
fi

echo "=== (b) exits non-zero ==="
if [ ${rc} -ne 0 ]; then
    ok "exit code ${rc} (non-zero, as expected for a 64-bit SIGNED field)"
else
    bad "exit code 0 (expected non-zero; was fststat fixed to handle 64-bit data?)"
fi

echo "=== (c) record output (for inspection) ==="
line=$(echo "${output}" | awk '$1 == "I64S" { print; exit }')
if [ -n "${line}" ]; then
    echo "${line}"
    ok "stats line for I64S captured"
else
    bad "no stats line for I64S"
fi
echo "--- warnings/errors ---"
echo "${output}" | grep -E 'WARNING|ERROR' || echo "(none)"

echo
echo "Summary: ${pass} passed, ${fail} failed."
if [ "${fail}" -ne 0 ]; then
    echo "Some fststat special checks FAILED."
    exit 1
fi
echo "All fststat special checks passed."
exit 0
