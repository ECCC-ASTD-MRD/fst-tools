#!/usr/bin/env bash
# Test fstcomp's exit code when a record's data differs but is NOT the last
# record compared.
#
# The two input files (09_fstcomp_ecode_{a,b}_in.rsf) contain the same three
# records (identical metadata); only REC3's data differs (base in A, diff in B).
# The records are written in a DIFFERENT order in the two files:
#   file A: REC1, REC2, REC3   (REC3 last)
#   file B: REC3, REC1, REC2   (REC3 first)
#
# fstcomp compares records in the order of file A, and (bug) its exit code
# reflects only the LAST record compared, not any difference. So:
#   run 1: -a A -b B  -> compares REC1, REC2, REC3; REC3 (diff) is last  -> rc=2
#   run 2: -a B -b A  -> compares REC3, REC1, REC2; REC3 (diff) is first -> rc=0 (bug)
#
# A correct fstcomp returns rc=2 in BOTH runs (a difference exists in both).
# This test asserts that; it fails until the exit-code bug is fixed.
#
# Usage: 02_fstcomp_ecode.sh <fstcomp>

set -u

if [ $# -lt 1 ]; then
    echo "Need argument to specify the fstcomp executable"
    exit 1
fi
FSTCOMP="$1"

file_a=09_fstcomp_ecode_a_in.rsf
file_b=09_fstcomp_ecode_b_in.rsf
for f in "${file_a}" "${file_b}"; do
    if [ ! -f "${f}" ]; then
        echo "Test FST file ${f} does not exist!"
        exit 1
    fi
done

echo "FSTCOMP = ${FSTCOMP}"

pass=0
fail=0
ok()   { pass=$((pass + 1)); echo "PASS: $1"; }
bad()  { fail=$((fail + 1)); echo "FAIL: $1"; }

# The stats line for a record, matched on the first field. A record whose data
# differs has its nomvar wrapped in angle brackets ("<REC3>"); an identical
# record has a plain, space-padded nomvar ("REC1").
get_line() {
    echo "$1" | awk -v f="$2" '$1 == f { print; exit }'
}

# Run fstcomp with the given -a/-b files; check that REC3 is reported as
# DIFFERENT, REC1/REC2 as SAME, and that the exit code is 2 (a difference
# exists). $1 = label, $2 = file for -a, $3 = file for -b.
run_and_check() {
    local label=$1 fa=$2 fb=$3
    echo "=== ${label}: -a ${fa} -b ${fb} ==="
    local output rc
    output=$("${FSTCOMP}" -a "${fa}" -b "${fb}" -ecode 2>&1)
    rc=$?

    if [ -n "$(get_line "${output}" "<REC3>")" ]; then
        ok "${label}: REC3 reported as DIFFERENT"
    else
        bad "${label}: REC3 not reported as DIFFERENT"
    fi
    local nv
    for nv in REC1 REC2; do
        if [ -n "$(get_line "${output}" "${nv}")" ]; then
            ok "${label}: ${nv} reported as SAME"
        else
            bad "${label}: ${nv} not reported as SAME"
        fi
    done
    if [ ${rc} -eq 2 ]; then
        ok "${label}: exit code 2 (difference found)"
    else
        bad "${label}: exit code ${rc} (expected 2; a difference exists but is not the last record compared)"
    fi
}

run_and_check "run 1" "${file_a}" "${file_b}"
run_and_check "run 2" "${file_b}" "${file_a}"

echo
echo "Summary: ${pass} passed, ${fail} failed."
if [ "${fail}" -ne 0 ]; then
    echo "Some fstcomp exit-code checks FAILED."
    exit 1
fi
echo "All fstcomp exit-code checks passed."
exit 0
