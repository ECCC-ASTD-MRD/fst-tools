#!/usr/bin/env bash
# Test fstcomp's per-record comparison of two files that differ only in the
# data of their records.
#
# The two input files (08_fstcomp_nonfinite_{a,b}_in.rsf) contain the same
# seven records (identical metadata); only the data of some records differs:
#
#   SAME  identical data            -> control: must be reported as SAME
#   FINC  4 vs 9 (finite)           -> control: must be reported as DIFFERENT
#   NANF  4 vs NaN                  -> must be reported as DIFFERENT
#   INFF  4 vs +Inf                 -> must be reported as DIFFERENT
#   NINF  4 vs -Inf                 -> must be reported as DIFFERENT
#   NANV  NaN vs +Inf               -> must be reported as DIFFERENT
#   INNV  +Inf vs -Inf              -> must be reported as DIFFERENT
#
# fstcomp prints one line per record; a record whose data differs is marked
# with <nomvar> (angle brackets) instead of a plain, space-padded nomvar. The
# test checks that marker for every record, and that the exit code is 2
# (differences found; the last record, INNV, differs).
#
# This test also guards a former fstcomp bug: comparing a point where either
# value is non-finite (NaN or +/-Inf) used to be reported as "equal". It now
# passes.
#
# Usage: 01_fstcomp_nonfinite.sh <fstcomp>

set -u

if [ $# -lt 1 ]; then
    echo "Need argument to specify the fstcomp executable"
    exit 1
fi
FSTCOMP="$1"

file_a=08_fstcomp_nonfinite_a_in.rsf
file_b=08_fstcomp_nonfinite_b_in.rsf
for f in "${file_a}" "${file_b}"; do
    if [ ! -f "${f}" ]; then
        echo "Test FST file ${f} does not exist!"
        exit 1
    fi
done

echo "FSTCOMP = ${FSTCOMP}"
echo "Comparing ${file_a} vs ${file_b}"

output=$("${FSTCOMP}" -a "${file_a}" -b "${file_b}" -ecode 2>&1)
rc=$?

pass=0
fail=0
ok()   { pass=$((pass + 1)); echo "PASS: $1"; }
bad()  { fail=$((fail + 1)); echo "FAIL: $1"; }

# The stats line for a record, matched on the first field. A record whose
# data differs has its nomvar wrapped in angle brackets ("<NANF>"); an
# identical record has a plain, space-padded nomvar ("SAME").
get_line() {
    echo "${output}" | awk -v f="$1" '$1 == f { print; exit }'
}

# $1 = nomvar expected to be reported as DIFFERENT
check_diff() {
    local nv=$1 line
    line=$(get_line "<${nv}>")
    if [ -n "${line}" ]; then
        ok "${nv}: reported as DIFFERENT"
    else
        bad "${nv}: not reported as DIFFERENT"
        line=$(get_line "${nv}")
        [ -n "${line}" ] && echo "    ${line}"
    fi
}

# $1 = nomvar expected to be reported as SAME
check_same() {
    local nv=$1 line
    line=$(get_line "${nv}")
    if [ -n "${line}" ]; then
        ok "${nv}: reported as SAME"
    else
        bad "${nv}: not reported as SAME"
        line=$(get_line "<${nv}>")
        [ -n "${line}" ] && echo "    ${line}"
    fi
}

echo "=== controls ==="
check_same SAME
check_diff FINC

echo "=== non-finite differences (should all be DIFFERENT) ==="
check_diff NANF
check_diff INFF
check_diff NINF
check_diff NANV
check_diff INNV

echo "=== exit code ==="
if [ ${rc} -eq 2 ]; then
    ok "exit code 2 (differences found)"
else
    bad "exit code ${rc} (expected 2)"
fi

echo
echo "Summary: ${pass} passed, ${fail} failed."
if [ "${fail}" -ne 0 ]; then
    echo "Some fstcomp non-finite checks FAILED."
    exit 1
fi
echo "All fstcomp non-finite checks passed."
exit 0
