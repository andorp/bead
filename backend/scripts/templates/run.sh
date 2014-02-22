#!/bin/sh

__MESSAGE=

say() {
    __MESSAGE="${__MESSAGE}$1\n"
}

export PATH=$PATH:/usr/local/bin
ulimit -t 5
cd /bead/run
. ./script
run
__RUN_RESULT=$?
if [ "${__MESSAGE}" != "" ]; then
    echo -e ${__MESSAGE} > .message
fi
exit ${__RUN_RESULT}
