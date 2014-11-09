#!/bin/sh

PENDING=".pending"
WATCHDOG_TIMEOUT=5

msg_n() {
    local tz

    tz="UTC"
    date=`env TZ=$tz date '+%Y.%m.%d. %H:%M:%S'`
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
