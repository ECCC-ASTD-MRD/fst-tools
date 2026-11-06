#!/usr/bin/env bash
# Test voir's -style option.
#
# The table-of-contents columns always appear in the same order, but their
# width may change in the future, so we do NOT compare the raw output. Instead
# we extract the header line (the line with the column labels), collapse all
# whitespace runs to single spaces, and compare the resulting sequence of
# column labels. This is order-aware but width-tolerant.
#
# Model (see src/voir/README.md):
#   - a non-empty -style starts from a BASE STATE; each recognized token
#     overrides it; unrecognized tokens are ignored;
#   - an ABSENT -style shows the DEFAULT columns;
#   - a BARE -style (present, no value) shows the 20.0 bare default.
#
# We check:
#   1. -style absent  -> the expected default columns
#   2. bare -style    -> the expected 20.0 bare-default columns
#   3. base state     -> an unrecognized token leaves the base columns
#   4. every ON token -> adds its column(s) relative to the base state
#   5. every OFF token-> removes its column(s) relative to the base state
#   6. aliases        -> each 20.0 alias produces the same header as its
#                        canonical token
#   7. FULL           -> shows everything interesting (all three IPs on)
set -u

if [ $# -lt 1 ]; then
    echo "Need argument to specify the voir executable"
    exit 1
fi
VOIR="$1"

# Prefer the XDF file (works with both the 20.0 and 20.1 backends); fall back
# to the RSF file if only that one is present.
test_file_in=03_voir_basic_in.xdf
if [ ! -f "${test_file_in}" ] && [ -f 03_voir_basic_in.rsf ]; then
    test_file_in=03_voir_basic_in.rsf
fi
if [ ! -f "${test_file_in}" ]; then
    echo "Test FST file 03_voir_basic_in.{xdf,rsf} does not exist!"
    exit 1
fi

# Extract the table-of-contents header line (the last non-blank line before the
# first data row, i.e. the line with the column labels) and normalize it:
# collapse all whitespace runs to single spaces and trim the ends.
get_header() {
    "${VOIR}" -iment "${test_file_in}" "$@" 2>&1 | awk '
        {
            if (!done && $0 ~ /^[[:space:]]*[0-9]+- /) { done = 1 }
            else if (!done && NF > 0) { last = $0 }
        }
        END { if (done) print last }' | tr -s '[:space:]' ' ' | sed 's/^ //; s/ $//'
}

pass=0
fail=0
ok()   { pass=$((pass + 1)); echo "PASS: $1"; }
bad()  { fail=$((fail + 1)); echo "FAIL: $1"; }

# assert_header_eq <label> <expected> <actual>
assert_header_eq() {
    if [ "$2" = "$3" ]; then
        ok "$1"
    else
        bad "$1"
        echo "    expected: $2"
        echo "    actual:   $3"
    fi
}

# assert_has <label> <fixed-string> <normalized-header>
assert_has() {
    if printf '%s' "$3" | grep -qF -- "$2"; then
        ok "$1"
    else
        bad "$1 (missing '$2' in: $3)"
    fi
}

# assert_lacks <label> <fixed-string> <normalized-header>
assert_lacks() {
    if printf '%s' "$3" | grep -qF -- "$2"; then
        bad "$1 (unexpected '$2' in: $3)"
    else
        ok "$1"
    fi
}

# --- Expected headers (normalized) -----------------------------------------
# Base state: a non-empty -style with no recognized token.
base='NOMV TV ETIQUETTE IP1 IP2 IP3 DEET NPAS DTY SIZ'
# Default columns: -style absent.
absent='NOMV TV ETIQUETTE NI NJ NK (DATE-O h m s) IP1 IP2 IP3 DEET NPAS DTY SIZ G IG1 IG2 IG3 IG4'
# 20.0 bare default: bare -style (NINJNK+DATEV+LEVEL+GRIDINFO, IP1 on by default).
bare='NOMV TV ETIQUETTE NI NJ NK (DATE-V h m s) STAMP-V LEVEL IP1 IP2 IP3 DEET NPAS DTY SIZ G XG1 XG2 XG3 XG4'

echo "=== 1. -style absent -> default columns ==="
assert_header_eq "absent: default columns" "${absent}" "$(get_header)"

echo "=== 2. bare -style -> 20.0 bare default ==="
assert_header_eq "bare: 20.0 bare default" "${bare}" "$(get_header -style)"

echo "=== 3. base state (unrecognized token) ==="
assert_header_eq "base: base columns" "${base}" "$(get_header -style ZZZNOTOKEN)"

echo "=== 4. ON tokens add a column (relative to the base state) ==="
# check_on <token> <col-fixed-string>...
check_on() {
    local token="$1"; shift
    local hdr
    hdr=$(get_header -style "${token}")
    local col
    for col in "$@"; do
        assert_lacks "on ${token}: '${col}' absent in base" "${col}" "${base}"
        assert_has   "on ${token}: '${col}' added"          "${col}" "${hdr}"
    done
}
check_on NINJNK     ' NI ' ' NJ ' ' NK '
check_on DATEO      '(DATE-O'
check_on DATESTAMPO 'STAMP-O'
check_on DATEV      '(DATE-V' 'STAMP-V'
check_on LEVEL      ' LEVEL '
check_on DECODE     'DECODED IP1/IP2/IP3'
check_on GRID       'XG1' 'XG2' 'XG3' 'XG4'
check_on IG1234     'IG1' 'IG2' 'IG3' 'IG4'

echo "=== 5. OFF tokens remove a column (relative to the base state) ==="
# check_off <token> <col-fixed-string>...
check_off() {
    local token="$1"; shift
    local hdr
    hdr=$(get_header -style "${token}")
    local col
    for col in "$@"; do
        assert_has   "off ${token}: '${col}' present in base" "${col}" "${base}"
        assert_lacks "off ${token}: '${col}' removed"         "${col}" "${hdr}"
    done
}
check_off NONOMVAR 'NOMV'
check_off NOTYPVAR ' TV '
check_off NOETIKET 'ETIQUETTE'
check_off NOIP1    ' IP1 '
check_off NOIP2    ' IP2 '
check_off NOIP3    ' IP3 '
check_off NOIP23   ' IP2 ' ' IP3 '
check_off NOIPS    ' IP1 ' ' IP2 ' ' IP3 '
check_off NODEET   ' DEET '
check_off NONPAS   ' NPAS '
check_off NODATYP  ' DTY '
# SIZ is the last column in the base header, so it has no trailing space.
check_off NOSIZ    ' SIZ'

echo "=== 6. 20.0 aliases match their canonical token ==="
# check_alias <canonical> <alias>
check_alias() {
    local a b
    a=$(get_header -style "$1")
    b=$(get_header -style "$2")
    assert_header_eq "alias ${2} == ${1}" "${a}" "${b}"
}
check_alias NONOMVAR NONOMV
check_alias NOTYPVAR NOTYPV
check_alias NOETIKET NOETIQ
check_alias NODATYP  NODTY
check_alias DECODE   IPALL
check_alias GRID     GRIDINFO

echo "=== 7. FULL shows everything interesting ==="
# 20.1 FULL = NINJNK+DATEV+IPALL+GRIDINFO (all three IPs stay on; deviation from
# 20.0, whose FULL suppressed IP2/IP3).
full='NOMV TV ETIQUETTE NI NJ NK (DATE-V h m s) STAMP-V DECODED IP1/IP2/IP3 IP1 IP2 IP3 DEET NPAS DTY SIZ G XG1 XG2 XG3 XG4'
assert_header_eq "FULL: everything interesting" "${full}" "$(get_header -style FULL)"

echo
echo "Summary: ${pass} passed, ${fail} failed."
if [ "${fail}" -ne 0 ]; then
    echo "Some -style checks FAILED."
    exit 1
fi
echo "All -style checks passed."
exit 0
