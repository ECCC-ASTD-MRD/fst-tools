#!/usr/bin/env bash
# Compare the RSF and XDF files generated for the `fststat` "special" test's
# non-finite records (07_fststat_special_nf_in.{rsf,xdf}) with fstcomp.
#
# Both files contain the same six non-finite REAL/32 records (+Inf, -Inf, NaN,
# all-Inf, all-NaN, and NaN+Inf), so fstcomp should find them identical and
# exit 0. The 64-bit SIGNED record from the full special file is deliberately
# excluded: XDF cannot read 64-bit integers back, so it would make the two
# files differ (and is not a bug — 64-bit ints are simply unsupported in XDF).
#
# This test also guards a former fstcomp crash: comparing this file pair used
# to abort with a "floating invalid" error. It now passes.
#
# Usage: 03_fststat_special_comp.sh <fstcomp>

set -u

if [ $# -lt 1 ]; then
    echo "Need argument to specify the fstcomp executable"
    exit 1
fi
FSTCOMP="$1"

file_rsf=07_fststat_special_nf_in.rsf
file_xdf=07_fststat_special_nf_in.xdf
for f in "${file_rsf}" "${file_xdf}"; do
    if [ ! -f "${f}" ]; then
        echo "Test FST file ${f} does not exist!"
        exit 1
    fi
done

echo "FSTCOMP = ${FSTCOMP}"
echo "Comparing ${file_rsf} vs ${file_xdf}"

"${FSTCOMP}" -a "${file_rsf}" -b "${file_xdf}" -ecode
rc=$?

if [ ${rc} -eq 0 ]; then
    echo "PASS: fstcomp found the RSF and XDF files identical (exit code 0)"
    exit 0
fi

# Exit codes > 128 mean the process died from a signal (crash); anything else
# is a plain "files differ" / error result.
if [ ${rc} -gt 128 ]; then
    echo "FAIL: fstcomp crashed (exit code ${rc}, signal $((rc - 128)))"
else
    echo "FAIL: fstcomp reported a difference or error (exit code ${rc})"
fi
exit 1
