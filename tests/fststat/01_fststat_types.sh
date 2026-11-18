#!/usr/bin/env bash
# Test fststat on a file with one field of each FST data type that fststat
# processes without an error.
#
# For every record (32-bit REAL, REAL_IEEE, SIGNED, UNSIGNED, the lossy
# 24-bit REAL_OLD_QUANT and 16-bit REAL, and 64-bit REAL_IEEE, which only
# logs a warning), we check that fststat:
#   (a) exits 0,
#   (b) emits exactly one stats line for the record (grepped by nomvar), and
#   (c) reports the known mean/min/max (2.0/1.0/3.0) within a tight tolerance.
#
# We also check globally that no record was skipped (no "Unable to read
# record" warning).
#
# The 64-bit SIGNED field, which triggers an RMN error in fststat, is tested
# separately (02_fststat_special.sh).
#
# Usage: 01_fststat_types.sh <fststat>

set -u

if [ $# -lt 1 ]; then
    echo "Need argument to specify the fststat executable"
    exit 1
fi
FSTSTAT="$1"

# Prefer the XDF file (works with both the 20.0 and 20.1 backends); fall back
# to the RSF file if only that one is present.
test_file_in=05_fststat_types_in.xdf
if [ ! -f "${test_file_in}" ] && [ -f 05_fststat_types_in.rsf ]; then
    test_file_in=05_fststat_types_in.rsf
fi
if [ ! -f "${test_file_in}" ]; then
    echo "Test FST file 05_fststat_types_in.{xdf,rsf} does not exist!"
    exit 1
fi

echo "FSTSTAT = ${FSTSTAT}"
echo "Input file = ${test_file_in}"

# Run fststat once and capture the output (stdout + stderr).
output=$("${FSTSTAT}" -fst "${test_file_in}" 2>&1)
rc=$?
if [ ${rc} -ne 0 ]; then
    echo "fststat -fst ${test_file_in} returned non-zero exit code ${rc}:"
    echo "${output}"
    exit 1
fi

pass=0
fail=0
ok()   { pass=$((pass + 1)); echo "PASS: $1"; }
bad()  { fail=$((fail + 1)); echo "FAIL: $1"; }

# Extract the stats line for a given nomvar (the first field of the line).
get_line() {
    echo "${output}" | awk -v nv="$1" '$1 == nv { print; exit }'
}

# Count the stats lines for a given nomvar.
count_lines() {
    echo "${output}" | awk -v nv="$1" '$1 == nv' | wc -l
}

# Parse the Mean / Min / Max values from a stats line. The line has the form:
#   <nomvar> <typvar> <level> <ip2> <ip3> <datev> <etiket> Mean:<m> StDev:<s> Min:[(i,j):<min>] Max:[(i,j):<max>]
parse_mean() { echo "$1" | sed -n 's/.* Mean:[[:space:]]*\([0-9.eE+-]*\).*/\1/p'; }
parse_min()  { echo "$1" | sed -n 's/.*Min:\[([[:space:]]*[0-9]*,[[:space:]]*[0-9]*):[[:space:]]*\([0-9.eE+-]*\)\].*/\1/p'; }
parse_max()  { echo "$1" | sed -n 's/.*Max:\[([[:space:]]*[0-9]*,[[:space:]]*[0-9]*):[[:space:]]*\([0-9.eE+-]*\)\].*/\1/p'; }

# check_value <label> <what> <actual> <expected>
# Assert that <actual> is within 1e-6 of <expected>.
check_value() {
    local label="$1" what="$2" actual="$3" expected="$4"
    if [ -z "${actual}" ]; then
        bad "${label}: could not parse ${what}"
        return
    fi
    local within
    within=$(awk -v a="${actual}" -v e="${expected}" 'BEGIN { d = a - e; if (d < 0) d = -d; print (d < 1e-6) ? 1 : 0 }')
    if [ "${within}" -eq 1 ]; then
        ok "${label}: ${what}=${actual} (expected ${expected})"
    else
        bad "${label}: ${what}=${actual} (expected ${expected})"
    fi
}

# check_valid <nomvar> <label>
# Assert (b) and (c) for a valid record.
check_valid() {
    local nomvar="$1"
    local label="$2"
    local line
    line=$(get_line "${nomvar}")
    if [ -z "${line}" ]; then
        bad "${label}: no stats line for ${nomvar}"
        return
    fi
    # (b) exactly one stats line
    local count
    count=$(count_lines "${nomvar}")
    if [ "${count}" -ne 1 ]; then
        bad "${label}: expected 1 stats line for ${nomvar}, got ${count}"
    else
        ok "${label}: one stats line"
    fi
    # (c) mean/min/max within tolerance
    check_value "${label}" "mean" "$(parse_mean "${line}")" 2.0
    check_value "${label}" "min"  "$(parse_min  "${line}")" 1.0
    check_value "${label}" "max"  "$(parse_max  "${line}")" 3.0
}

echo "=== Global checks ==="
if echo "${output}" | grep -q "Unable to read record"; then
    bad "no record was skipped"
else
    ok "no record was skipped"
fi

echo "=== Types processed without error ==="
check_valid "R32F" "REAL/32"
check_valid "E32F" "REAL_IEEE/32"
check_valid "I32S" "SIGNED/32"
check_valid "I32U" "UNSIGNED/32"
check_valid "R24Q" "REAL_OLD_QUANT/24"
check_valid "R16F" "REAL/16"
check_valid "E64F" "REAL_IEEE/64"

echo
echo "Summary: ${pass} passed, ${fail} failed."
if [ "${fail}" -ne 0 ]; then
    echo "Some fststat checks FAILED."
    exit 1
fi
echo "All fststat checks passed."
exit 0
