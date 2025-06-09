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
    pwd
    ls -l ${test_file}
    if ! test -f ${test_file}; then
        echo "Test FST file ${test_file} does not exist!"
        exit -1
    fi

    directives=editfst.dir
    echo "desire('', '', '', -1, 3, -1, -1)" > ${directives}

    output_file_1=${test_file}.01
    rm -f ${output_file_1}
    ${EDITFST} -s ${test_file} -d ${output_file_1} -i ${directives} || exit -1

    output_file_2=${test_file}.02
    rm -f ${output_file_2}
    cat ${directives} | ${EDITFST} -s ${test_file} -d ${output_file_2} || exit -1

    ${FSTCOMP} -a ${output_file_1} -b ${output_file_2} -ecode || exit -1
    ${FSTCOMP} -a ${output_file_2} -b ${output_file_1} -ecode || exit -1

    output_file_3=${test_file}.03
    rm -f ${output_file_3}
    ${EDITFST} -s ${test_file} -d ${output_file_3} -i 0 || exit -1
    ${FSTCOMP} -a ${test_file} -b ${output_file_3} -ecode || exit -1
}

${FSTCOMP} -a test.rsf -b test.xdf -ecode || exit -1

run_test test.rsf
run_test test.xdf

exit 0
