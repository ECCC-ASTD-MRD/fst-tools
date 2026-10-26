#!/usr/bin/env bash
#
# Regression test for the `editfst -ds` (sequential destination) hang.
#
# When the destination is a sequential file, ouvred() opens it and then runs a
# loop that counts the existing records before the copy:
#
#     COPIES = query%find_next(record)
#     IF(COPIES .GE. 0) THEN ... GO TO 10
#
# `find_next` returns a LOGICAL (.true./.false.), but COPIES is an INTEGER. When
# no (more) records exist, .false. converts to 0, and `0 .GE. 0` is true, so the
# loop never terminates. This used to work because the pre-fst24 code called
# FSTINF(3,...), which returned an integer count (negative at end of search).
#
# The test runs `editfst -s <in> -d <out> -ds` under a timeout. With the bug the
# command spins at 100% CPU forever and the timeout kills it (non-zero exit);
# with the fix it completes in well under a second.
#
# Sequential files are an XDF-only concept, so the test uses an XDF input and an
# empty XDF destination (the exact repro from the bug report). The hang is in
# ouvred()'s record-counting loop, which runs before any format-specific code,
# so it is triggered by the -ds flag regardless of the destination's prior
# contents; the empty case hits it on the very first find_next.
#
# Note: a `-ds` output is a sequential file (records + a logical EOF mark), so
# it cannot be byte-compared with the random-access expected file using
# fstcomp/voir. We therefore verify that editfst completes and produces a
# non-empty output file.

if [ $# -lt 2 ]; then
    echo "Need arguments to specify editfst and fstcomp executables"
    exit -1
fi

EDITFST="${1}"
FSTCOMP="${2}"

echo "EDITFST = ${EDITFST}"
echo "FSTCOMP = ${FSTCOMP}"

# Generous enough for slow CI, short enough to fail fast on a hang. The CTest
# TIMEOUT (see tests/CMakeLists.txt) is a larger backstop in case the script
# itself hangs for some other reason.
TIMEOUT=5

# Run editfst with a sequential destination under a timeout.
# Returns 0 if editfst finished in time, non-zero if it hung (was killed).
# `timeout -k` sends SIGKILL after a grace period: a spinning Fortran runtime
# may ignore the initial SIGTERM, so we escalate to be sure the process dies.
run_editfst_ds() {
    local src=${1}
    local dst=${2}
    rm -f "${dst}"
    timeout -k 2 "${TIMEOUT}" "${EDITFST}" -s "${src}" -d "${dst}" -ds -i 0
    local rc=$?
    if [ ${rc} -eq 124 ] || [ ${rc} -eq 137 ]; then
        echo "FAIL: editfst -ds hung (killed after ${TIMEOUT}s) on ${dst}"
        return 1
    elif [ ${rc} -ne 0 ]; then
        echo "FAIL: editfst -ds returned unexpected exit code ${rc} on ${dst}"
        return 1
    fi
    return 0
}

# Compare RSF and XDF input files first (as the other editfst tests do).
if ! ${FSTCOMP} -a 04_editfst_ds_in.rsf -b 04_editfst_ds_in.xdf -ecode; then
    echo "RSF and XDF input files are different!"
    exit -1
fi

in_file=04_editfst_ds_in.xdf
if ! [ -f ${in_file} ]; then
    echo "Test FST file ${in_file} does not exist!"
    exit -1
fi

echo "=== xdf: empty sequential destination ==="
empty_dst=${in_file}.empty_ds
if ! run_editfst_ds ${in_file} ${empty_dst}; then
    exit -1
fi
if [ ! -s ${empty_dst} ]; then
    echo "FAIL: empty-destination output ${empty_dst} is missing or empty"
    exit -1
fi

echo "All editfst -ds tests passed."
exit 0
