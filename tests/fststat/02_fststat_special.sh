#!/usr/bin/env bash
# Test fststat on a file with "special" fields that fststat handles
# differently from the normal case.
#
# The file contains seven records, in this order:
#   I64S  64-bit SIGNED   -> RMN error, garbage values (fststat does not
#                            handle 64-bit data; to be fixed later)
#   PINF  REAL/32 +Inf    -> stats line with Infinity values + warning
#   NINF  REAL/32 -Inf    -> stats line with -Infinity values + warning
#   NANF  REAL/32 NaN     -> stats line with correct values (NaN skipped)
#                            + warning
#   AINF  REAL/32 all +Inf -> stats line with Inf mean, NaN stdev + warning
#   ANAN  REAL/32 all NaN  -> stats line with NaN mean/stdev + warning
#   MIXN  REAL/32 NaN+Inf  -> stats line with Inf mean, NaN stdev + both
#                            NaN and infinity warnings
#
# This test checks:
#   (a) I64S: a stats line is printed (garbage values) and the RMN error is
#       logged,
#   (b) PINF/NINF: a stats line is printed with Inf values and its own
#       "Number of infinities" warning,
#   (c) NANF: a stats line is printed with the correct mean (NaN is skipped)
#       and its own "Number of NaN" warning,
#   (d) AINF/ANAN/MIXN: a stats line is printed with the expected mean
#       (Inf / NaN / Inf) and its own warning(s),
#   (e) the process runs to completion (final "Status" banner present).
#
# Because fststat now prints each stats line and its warnings through the same
# (synchronized) logging channel, a record's warnings always appear right after
# its stats line, so each warning can be attributed to the correct record.
#
# The exit code is non-zero because of the I64S RMN error.
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

# Extract the stats line for a given nomvar (the first field of the line).
get_line() {
    echo "${output}" | awk -v nv="$1" '$1 == nv { print; exit }'
}

# Extract a record's stats line plus the warning lines that immediately follow
# it. This is only reliable because fststat now prints the stats line and its
# warnings through the same (synchronized) logging channel, so a record's
# warnings always come right after its stats line.
get_block() {
    echo "${output}" | awk -v nv="$1" '
        $1 == nv { found = 1; print; next }
        found && /^\(/ { print; next }
        found { exit }
    '
}

# The first token of the Mean: field (e.g. "0.200000E+01", "Infinity").
mean_field() { echo "$1" | sed -n 's/.* Mean:[[:space:]]*\([^ ]*\).*/\1/p'; }

echo "=== (a) I64S: 64-bit SIGNED -> RMN error, garbage values ==="
line=$(get_line "I64S")
if [ -n "${line}" ]; then
    ok "stats line for I64S present (garbage values expected)"
    echo "    ${line}"
else
    bad "no stats line for I64S"
fi
if echo "${output}" | grep -q "compact_u_integer"; then
    ok "RMN error 'compact_u_integer' logged"
else
    bad "RMN error 'compact_u_integer' not logged"
fi

echo "=== (b) PINF/NINF: infinities -> Inf values + warning ==="
for nv in PINF NINF; do
    line=$(get_line "${nv}")
    if [ -z "${line}" ]; then
        bad "no stats line for ${nv}"
        continue
    fi
    echo "    ${line}"
    mf=$(mean_field "${line}")
    if echo "${mf}" | grep -qi "inf"; then
        ok "${nv}: mean is Inf (${mf})"
    else
        bad "${nv}: mean is not Inf (got '${mf}')"
    fi
    block=$(get_block "${nv}")
    if echo "${block}" | grep -q "Number of infinities in record"; then
        ok "${nv}: its own 'Number of infinities' warning is attached"
    else
        bad "${nv}: no 'Number of infinities' warning attached to its stats line"
    fi
done

echo "=== (c) NANF: NaN -> correct values (NaN skipped) + warning ==="
line=$(get_line "NANF")
if [ -z "${line}" ]; then
    bad "no stats line for NANF"
else
    echo "    ${line}"
    # Mean of 1,2,3 with the NaN entry ignored: (1+2+3)/3 = 2.0
    mf=$(mean_field "${line}")
    within=$(awk -v a="${mf}" -v e="2.0" 'BEGIN { d = a - e; if (d < 0) d = -d; print (d < 1e-6) ? 1 : 0 }')
    if [ "${within}" -eq 1 ]; then
        ok "NANF: mean is 2.0 (NaN entry ignored)"
    else
        bad "NANF: mean is not 2.0 (got '${mf}')"
    fi
    block=$(get_block "NANF")
    if echo "${block}" | grep -q "Number of NaN in record"; then
        ok "NANF: its own 'Number of NaN' warning is attached"
    else
        bad "NANF: no 'Number of NaN' warning attached to its stats line"
    fi
fi

echo "=== (d) AINF/ANAN/MIXN: all-non-finite records ==="
# AINF (all +Inf): mean is Inf, stdev is NaN (no finite values to vary).
line=$(get_line "AINF")
if [ -z "${line}" ]; then
    bad "no stats line for AINF"
else
    echo "    ${line}"
    mf=$(mean_field "${line}")
    if echo "${mf}" | grep -qi "inf"; then
        ok "AINF: mean is Inf (${mf})"
    else
        bad "AINF: mean is not Inf (got '${mf}')"
    fi
    block=$(get_block "AINF")
    if echo "${block}" | grep -q "Number of infinities in record"; then
        ok "AINF: its own 'Number of infinities' warning is attached"
    else
        bad "AINF: no 'Number of infinities' warning attached to its stats line"
    fi
fi
# ANAN (all NaN): mean and stdev are NaN (no finite values at all).
line=$(get_line "ANAN")
if [ -z "${line}" ]; then
    bad "no stats line for ANAN"
else
    echo "    ${line}"
    mf=$(mean_field "${line}")
    if echo "${mf}" | grep -qi "nan"; then
        ok "ANAN: mean is NaN (${mf})"
    else
        bad "ANAN: mean is not NaN (got '${mf}')"
    fi
    block=$(get_block "ANAN")
    if echo "${block}" | grep -q "Number of NaN in record"; then
        ok "ANAN: its own 'Number of NaN' warning is attached"
    else
        bad "ANAN: no 'Number of NaN' warning attached to its stats line"
    fi
fi
# MIXN (NaN + Inf, no finite): mean is Inf (Inf/2), stdev is NaN.
line=$(get_line "MIXN")
if [ -z "${line}" ]; then
    bad "no stats line for MIXN"
else
    echo "    ${line}"
    mf=$(mean_field "${line}")
    if echo "${mf}" | grep -qi "inf"; then
        ok "MIXN: mean is Inf (${mf})"
    else
        bad "MIXN: mean is not Inf (got '${mf}')"
    fi
    # MIXN has both NaN and infinities, so it must log BOTH warnings.
    block=$(get_block "MIXN")
    if echo "${block}" | grep -q "Number of NaN in record"; then
        ok "MIXN: its own 'Number of NaN' warning is attached"
    else
        bad "MIXN: no 'Number of NaN' warning attached to its stats line"
    fi
    if echo "${block}" | grep -q "Number of infinities in record"; then
        ok "MIXN: its own 'Number of infinities' warning is attached"
    else
        bad "MIXN: no 'Number of infinities' warning attached to its stats line"
    fi
fi

echo "=== (e) process ran to completion ==="
if echo "${output}" | grep -qE '^Status[[:space:]]*:'; then
    ok "final Status banner present"
else
    bad "final Status banner missing (did fststat crash?)"
fi
if [ ${rc} -ne 0 ]; then
    ok "exit code ${rc} (non-zero, expected due to I64S RMN error)"
else
    bad "exit code 0 (expected non-zero due to I64S RMN error)"
fi

echo
echo "Summary: ${pass} passed, ${fail} failed."
if [ "${fail}" -ne 0 ]; then
    echo "Some fststat special checks FAILED."
    exit 1
fi
echo "All fststat special checks passed."
exit 0
