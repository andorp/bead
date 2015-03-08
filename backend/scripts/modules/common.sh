#!/bin/sh

SCRIPT_PATH=$(realpath $0)
SCRIPT_PREFIX=$(dirname ${SCRIPT_PATH})
BEAD_CONF=${SCRIPT_PREFIX}/../../etc/bead.conf

PENDING=".pending"
LOCKED=".locked"

msg_n() {
    local tz

    tz="UTC"
    date=$(env TZ=$tz date '+%Y.%m.%d. %H:%M:%S')
    echo -n "[${date}] $1";
}

msg() {
    msg_n "$1";
    echo ""
}

msg_verbose() {
    [ ${VERBOSE:-0} -gt 0 ] || return 0
    msg "$1"
}

msg_debug() {
    [ ${VERBOSE:-0} -gt 1 ] || return 0
    msg "DEBUG: $1" >&2
}

msg_debug "Checking for ${BEAD_CONF}..."

if [ -f ${BEAD_CONF} ]; then
    . ${BEAD_CONF}
    msg_debug "OK"
else
    msg_debug "NOT FOUND"
fi

# Some reasonable default
: ${WATCHDOG_TIMEOUT:=60}
: ${SLEEP_TIME:=5}
: ${BEAD_HOME:=/usr/home/bead}

msg_debug "Watchdog timeout = ${WATCHDOG_TIMEOUT}"
msg_debug "Sleep time = ${SLEEP_TIME}"
msg_debug "BE-AD home = ${BEAD_HOME}"
