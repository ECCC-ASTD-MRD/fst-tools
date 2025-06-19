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
    echo "desire('', '', '', -1, 3, -1, -1)" > ${directives}

    echo "Copy single record (directive from file)"
    output_file_1=${test_file_in}.01
    rm -f ${output_file_1}
    ${EDITFST} -s ${test_file_in} -d ${output_file_1} -i ${directives} || exit -1
    ${FSTCOMP} -a ${test_file_expected} -b ${output_file_1} -ecode || exit -1
    ${FSTCOMP} -b ${test_file_expected} -a ${output_file_1} -ecode || exit -1

    echo "Copy single record (directive from stdin)"
    output_file_2=${test_file_in}.02
    rm -f ${output_file_2}
    cat ${directives} | ${EDITFST} -s ${test_file_in} -d ${output_file_2} || exit -1
    ${FSTCOMP} -a ${test_file_expected} -b ${output_file_2} -ecode || exit -1
    ${FSTCOMP} -b ${test_file_expected} -a ${output_file_2} -ecode || exit -1

    echo "Copy the entire file"
    output_file_3=${test_file_in}.03
    rm -f ${output_file_3}
    ${EDITFST} -s ${test_file_in} -d ${output_file_3} -i 0 || exit -1
    ${FSTCOMP} -a ${test_file_in} -b ${output_file_3} -ecode || exit -1
}

# Compare RSF and XDF test files
if ! ${FSTCOMP} -a 01_editfst_desire_in.rsf -b 01_editfst_desire_in.xdf -ecode; then
    echo "RSF and XDF input files are different!"
    exit -1
fi

run_test 01_editfst_desire_in.rsf 01_editfst_desire_out.rsf
run_test 01_editfst_desire_in.xdf 01_editfst_desire_out.xdf

exit 0
