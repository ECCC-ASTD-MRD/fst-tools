if [[ "${ORDENV_SETUP}" == "" ]] ; then
    echo "ERROR: Ordenv must be setup to work on pgsm"
    return 1
fi
. ssmuse-sh -d main/opt/cmake/cmake-3.16.4
. ssmuse-sh -x hpco/exp/intelpsxe-cluster-19.0.3.199
. r.load.dot rpn/code-tools/1.5.0 
. r.load.dot rpn/utils/19.7.0 rpn/libs/19.7.0
# source /home/phc001/workspace/spooki/SETUP_ubuntu-18.04-amd64-64

