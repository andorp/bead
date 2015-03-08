#!/bin/sh

usage() {
    echo "bead bulk [jailname]

Parameters:
    jailname     -- Jail to use
"
    exit 1
}

JAILNAME="$1"

test -z "${JAILNAME}" && usage

SCRIPT_PATH=$(realpath $0)
SCRIPT_PREFIX=$(dirname ${SCRIPT_PATH})

. ${SCRIPT_PREFIX}/common.sh

INCOMING_DIR="${BEAD_HOME}/jobs/${JAILNAME}/incoming"

if [ ! -d "${INCOMING_DIR}" ]; then
    msg "The ${INCOMING_DIR} cannot be found."
    exit 1
fi

JAIL_PATH="${BEAD_HOME}/jails/${JAILNAME}"

if [ ! -d "${JAIL_PATH}" ]; then
    msg "Jail ${JAILNAME} cannot be found."
    exit 1
fi

LOOP=1

main_loop() {
    local id

    while [ "${LOOP}" -ne "0" ]; do
        id=$(ls ${INCOMING_DIR} | fgrep -v ${PENDING} | fgrep -v ${LOCKED} | head -1)
        if [ "${id}" = "" ]; then
            msg_verbose "No job found, sleeping for ${SLEEP_TIME} seconds."
            sleep ${SLEEP_TIME}
        else
            msg "Found job ${id}, evaluating."
            /bin/sh ${SCRIPT_PREFIX}/test.sh ${JAILNAME} ${id}
        fi
    done
}

signal_handler() {
    LOOP=0
}

trap signal_handler SIGINT
trap signal_handler SIGTERM
trap signal_handler SIGKILL
trap signal_handler EXIT

msg "Bulk mode started with jail ${JAILNAME}."
main_loop
msg "Bulk mode stopped with jail ${JAILNAME}."
