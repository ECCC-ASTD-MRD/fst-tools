#!/usr/bin/env bash
# Test voir's -style option.
#
# The table-of-contents columns always appear in the same order, but their
# width may change in the future, so we do NOT compare the raw output. Instead
# we extract the header line (the line with the column labels), collapse all
# whitespace runs to single spaces, and compare the resulting sequence of
# column labels. This is order-aware but width-tolerant.
#
# We check:
#   1. -style absent  -> the expected default columns are printed
#   2. -style empty   -> the expected base columns are printed
#   3. every field    -> each field can be turned off (if on by default) or
#                        on (if off by default) with the corresponding token
#
# Note: the -style string is parsed by fst24_make_fields_from_string, which
# starts from a base state and only overrides the components matching a token.
# The on/off checks below are therefore expressed *relative* to the empty-style
# baseline captured at run time, so they do not depend on the absolute base
# state.
set -u

if [ $# -lt 1 ]; then
    echo "Need argument to specify the voir executable"
    exit 1
fi
VOIR="$1"

test_file_in=03_voir_basic_in.rsf
if [ ! -f "${test_file_in}" ]; then
    echo "Test FST file ${test_file_in} does not exist!"
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

echo "=== 1. -style absent ==="
hdr_absent=$(get_header)
expected_absent='NOMV TV ETIQUETTE NI NJ NK (DATE-O h m s) IP1 IP2 IP3 DEET NPAS DTYP SIZ G IG1 IG2 IG3 IG4'
assert_header_eq "absent: default columns" "${expected_absent}" "${hdr_absent}"

echo "=== 2. -style present but empty ==="
hdr_empty=$(get_header -style "")
expected_empty='NOMV TV ETIQUETTE NI NJ NK DATE-O IP1 IP2 IP3 DTYP SIZ G IG1 IG2 IG3 IG4'
assert_header_eq "empty: base columns" "${expected_empty}" "${hdr_empty}"

echo "=== 3. turn off fields that are on by default ==="
# For each NO* token, the target column(s) must be present in the empty baseline
# and absent when the token is given.
check_off() {  # check_off <token> <col-fixed-string>...
    local token="$1"; shift
    local hdr
    hdr=$(get_header -style "${token}")
    local col
    for col in "$@"; do
        assert_has   "off ${token}: '${col}' present in baseline" "${col}" "${hdr_empty}"
        assert_lacks "off ${token}: '${col}' removed"            "${col}" "${hdr}"
    done
}
check_off NONOMVAR 'NOMV'
check_off NOTYPVAR ' TV '
check_off NOETIKET 'ETIQUETTE'
check_off NONIJK   ' NI ' ' NJ ' ' NK '
check_off NODATEO  'DATE-O'
check_off NODATYP  'DTYP'
check_off NOIP1    ' IP1 '
check_off NOIP2    ' IP2 '
check_off NOIP3    ' IP3 '
check_off NOIPS    ' IP1 ' ' IP2 ' ' IP3 '
check_off NOIGS    'IG1'

echo "=== 4. turn on fields that are off by default ==="
# For each ON token, the target column(s) must be absent in the empty baseline
# and present when the token is given.
check_on() {  # check_on <token> <col-fixed-string>...
    local token="$1"; shift
    local hdr
    hdr=$(get_header -style "${token}")
    local col
    for col in "$@"; do
        assert_lacks "on ${token}: '${col}' absent in baseline" "${col}" "${hdr_empty}"
        assert_has   "on ${token}: '${col}' added"             "${col}" "${hdr}"
    done
}
check_on DATEV  ' DATE-V '
check_on LEVEL  ' LEVEL '
check_on DEET   ' DEET '
check_on NPAS   ' NPAS '
check_on DECODE 'IPs'
check_on GRID   'XG1'

echo "=== 5. NOSTAMP changes the DATE-O format ==="
# NOSTAMP turns off the date stamps: the DATE-O column changes from the compact
# 'DATE-O' label to the expanded '(DATE-O  h m s)' label.
hdr_stamp=$(get_header -style "NOSTAMP")
assert_has   "NOSTAMP: baseline DATE-O is stamped"     ' DATE-O ' "${hdr_empty}"
assert_lacks "NOSTAMP: stamped DATE-O label removed"   ' DATE-O ' "${hdr_stamp}"
assert_has   "NOSTAMP: un-stamped (DATE-O h m s) used" '(DATE-O'  "${hdr_stamp}"

echo
echo "Summary: ${pass} passed, ${fail} failed."
if [ "${fail}" -ne 0 ]; then
    echo "Some -style checks FAILED."
    exit 1
fi
echo "All -style checks passed."
exit 0
