#!/bin/sh

__MESSAGE=

say() {
    __MESSAGE=${__MESSAGE}$1$'\n'
}

export PATH=$PATH:/usr/local/bin
SANDBOX_PATH="$1"
ULIMIT="$2"
ulimit -t ${ULIMIT}
cd ${SANDBOX_PATH}
. ./script
run
__RUN_RESULT=$?
if [ "${__MESSAGE}" != "" ]; then
    echo "${__MESSAGE}" > ${SANDBOX_PATH}/.message
fi
pkill -9 -U 65534
exit ${__RUN_RESULT}
