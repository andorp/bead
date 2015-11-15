#!/bin/sh

SCRIPT_PATH=$(realpath $0)
SCRIPT_PREFIX=$(dirname ${SCRIPT_PATH})

. ${SCRIPT_PREFIX}/common.sh

PID="$1"
OUTPUT_DIR="$2"
OUTPUT="$3"
MESSAGE="$4"
RESULT="$5"
BUILDBOX="$6"
SANDBOX="$7"
TIMEOUT="$8"
OUTPUT_DIR_TMP=$(dirname ${OUTPUT})

test -z "$PID" && exit 1
test -z "$OUTPUT_DIR" && exit 1
test -z "$OUTPUT" && exit 1

sleep ${TIMEOUT}

if kill -9 $PID > /dev/null 2>&1; then
    if ! pkill -9 -U 65534; then
        msg "[watchdog] Could not stop processes owned by nobody."
    else
        msg "[watchdog] Had to kill nobody's all processes."
        mkdir -p ${OUTPUT_DIR_TMP}
        chmod g+rw,o+rw ${OUTPUT_DIR_TMP}
        watchdog_output=$(mktemp)
        watchdog_message=$(mktemp)
        watchdog_result=$(mktemp)
        echo "Testing of this process has exceeded the time limit of ${WATCHDOG_TIMEOUT} seconds." > ${watchdog_output} 2>&1
        echo "Sorry, but automated testing of the solution was given up due to resource limits." > ${watchdog_message} 2>&1
        echo "False" > ${watchdog_result} 2>&1
        force_publish ${watchdog_output} ${OUTPUT}
        force_publish ${watchdog_message} ${MESSAGE}
        force_publish ${watchdog_result} ${RESULT}
        mv ${OUTPUT_DIR_TMP} ${OUTPUT_DIR}
        rm -rf ${BUILDBOX}
        rm -rf ${SANDBOX}
    fi
fi
