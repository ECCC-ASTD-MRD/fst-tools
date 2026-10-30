#!/usr/bin/env bash
#
# Verify that the backend type (RSF vs XDF) of a file created by editfst is
# controlled by the BACKEND option of the FST_OPTIONS environment variable, and
# that the default (no FST_OPTIONS) is RSF.
#
# For each scenario, editfst copies the input file to a fresh destination and
# the backend of that destination is read with the Python rmn module
# (fst24_file(...).backend_name) and compared to the expected value. Opening an
# existing file for reading auto-detects its backend from the file, so the read
# is independent of FST_OPTIONS.
#
# Usage: 05_editfst_backend.sh <editfst> <python>
#
# The rmn Python module is located via the PYTHONPATH environment variable,
# which is set by CTest (see tests/CMakeLists.txt). The rmn lib directory (for
# librmn.so's dependencies) is derived from PYTHONPATH inside get_backend().

if [ $# -lt 2 ]; then
    echo "Need arguments to specify the editfst executable and the Python interpreter"
    exit 1
fi

EDITFST="${1}"
PYTHON="${2}"

echo "EDITFST = ${EDITFST}"
echo "PYTHON = ${PYTHON}"

# Input file produced by the generate_fst fixture (see tests/generate_fst.py).
# Its own backend is irrelevant: the output backend is set by FST_OPTIONS.
in_file=01_editfst_desire_in.rsf
if [ ! -f ${in_file} ]; then
    echo "Test FST file ${in_file} does not exist!"
    exit 1
fi

# Print the backend name of an FST file using the Python rmn module. The
# filename is passed as argv[1] to avoid shell-quoting issues.
#
# The rmn Python module loads librmn.so via a path relative to its own install
# location, but librmn.so depends on other rmn libraries (e.g. libApp.so) that
# live in the rmn lib directory. That directory is the parent of the first
# PYTHONPATH entry, so it is prepended to LD_LIBRARY_PATH for the Python
# subprocess only. This leaves editfst's inherited LD_LIBRARY_PATH (which carries
# the compiler runtime, e.g. libimf.so) untouched.
get_backend() {
    local file="$1"
    local rmn_lib_dir
    rmn_lib_dir="$(dirname "${PYTHONPATH%%:*}")"
    LD_LIBRARY_PATH="${rmn_lib_dir}:${LD_LIBRARY_PATH}" \
        "${PYTHON}" -c 'import rmn, sys; print(rmn.fst24_file(sys.argv[1], "R/O").backend_name)' "${file}"
}

# Each entry is "FST_OPTIONS value|expected backend"; an empty value means the
# variable is unset.
scenarios=(
    "|RSF"
    "BACKEND=XDF|XDF"
    "BACKEND=RSF|RSF"
)

for scenario in "${scenarios[@]}"; do
    options="${scenario%%|*}"
    expected="${scenario##*|}"

    out_file=${in_file}.backend
    rm -f ${out_file}

    if [ -n "${options}" ]; then
        if ! FST_OPTIONS="${options}" "${EDITFST}" -s ${in_file} -d ${out_file} -i 0; then
            echo "FAIL: editfst failed (FST_OPTIONS='${options}')"
            exit 1
        fi
    else
        if ! env -u FST_OPTIONS "${EDITFST}" -s ${in_file} -d ${out_file} -i 0; then
            echo "FAIL: editfst failed (FST_OPTIONS unset)"
            exit 1
        fi
    fi

    if ! actual=$(get_backend "${out_file}"); then
        echo "FAIL: could not read backend of ${out_file} (FST_OPTIONS='${options}')"
        exit 1
    fi
    if [ "${actual}" != "${expected}" ]; then
        echo "FAIL: FST_OPTIONS='${options}' ${out_file}: expected backend '${expected}', got '${actual}'"
        exit 1
    fi
    echo "OK: FST_OPTIONS='${options}' ${out_file} -> ${actual}"
done

exit 0
