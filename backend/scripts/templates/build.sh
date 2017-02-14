#!/bin/sh

show_program() {
    cat "$1" | sed = | sed 'N; s/^/    /; s/ *\(.\{4,\}\)\n/\1  /'
}

__MESSAGE=

say() {
    __MESSAGE=${__MESSAGE}$1$'\n'
}

export PATH=$PATH:/usr/local/bin
SANDBOX_PATH="$1"
BUILD_PATH="$2"
ULIMIT="$3"
ulimit -t ${ULIMIT}
cd ${BUILD_PATH}
. ./script
build
__BUILD_RESULT=$?
if [ "${__MESSAGE}" != "" ]; then
    echo "${__MESSAGE}" > ${BUILD_PATH}/.message
fi
exit ${__BUILD_RESULT}
