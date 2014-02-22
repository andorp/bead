#!/bin/sh

SCRIPT_PATH=`realpath $0`
SCRIPT_PREFIX=`dirname ${SCRIPT_PATH}`

. ${SCRIPT_PREFIX}/common.sh

PID="$1"
OUTPUT="$2"

test -z "$PID" && exit 1
test -z "$OUTPUT" && exit 1

sleep ${WATCHDOG_TIMEOUT}

if ps -p $PID > /dev/null 2>&1; then
    if ! kill -9 $PID > /dev/null 2>&1; then
        msg "[watchdog] Could not send SIGKILL to process $PID."
    else
        msg "[watchdog] Had to kill $PID."
        echo "Testing of this process has exceeded the time limit of ${WATCHDOG_TIMEOUT} seconds." > ${OUTPUT} 2>&1
    fi
fi
