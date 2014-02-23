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
ulimit -t 5
cd /bead/build
. ./script
build
__BUILD_RESULT=$?
if [ "${__MESSAGE}" != "" ]; then
    echo "${__MESSAGE}" > .message
fi
exit ${__BUILD_RESULT}
