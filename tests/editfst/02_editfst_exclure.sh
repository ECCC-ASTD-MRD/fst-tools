#!/usr/bin/env bash

if [ $# -lt 2 ]; then
    echo "Need arguments to specify editfst and fstcomp executables"
    exit -1
fi

EDITFST="${1}"
FSTCOMP="${2}"

echo "EDITFST = ${EDITFST}"
echo "FSTCOMP = ${FSTCOMP}"

function run_test() {
    test_file=${1:-test.fst}
    if ! test -f ${test_file}; then
        echo "Test FST file ${test_file} does not exist!"
        exit -1
    fi

    directives=editfst.dir
    echo "exclure(-1,['TT','HU'])" > ${directives}

    output_file_1=${test_file}.01
    rm -f ${output_file_1}
    # Currently, we only check that it doesn't crash when using that directive
    ${EDITFST} -s ${test_file} -d ${output_file_1} -i ${directives} || exit -1
}

run_test test.rsf
run_test test.xdf

exit 0
