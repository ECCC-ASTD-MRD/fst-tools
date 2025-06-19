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
    # Function inputs: test files
    test_file_in=${1:-test.fst}
    test_file_expected=${2:-test.fst}
    if ! [ -f ${test_file_in} ] || ! [ -f ${test_file_expected} ]; then
        echo "Test FST file ${test_file_in}/${test_file_expected} does not exist!"
        exit -1
    fi

    directives=editfst.dir
    echo "exclure(-1,['TT','HU'])" > ${directives}

    output_file_1=${test_file_in}.01
    rm -f ${output_file_1}
    ${EDITFST} -s ${test_file_in} -d ${output_file_1} -i ${directives} || exit -1
    ${FSTCOMP} -a ${test_file_expected} -b ${output_file_1} -ecode || exit -1
    ${FSTCOMP} -b ${test_file_expected} -a ${output_file_1} -ecode || exit -1
}

run_test 02_editfst_exclure_in.rsf 02_editfst_exclure_out.rsf
run_test 02_editfst_exclure_in.xdf 02_editfst_exclure_out.xdf

exit 0
