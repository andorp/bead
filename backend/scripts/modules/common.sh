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

check_encoding() {
    local f
    local g
    local r

    f=$1

    [ ! -s $f ] && return 0

    g=$(mktemp)
    (echo "This is a workaround for file(1), you should not see it."; cat $f) > $g
    case $(file -b $g) in
      *ASCII\ text*) r=0;;
      *UTF-8\ Unicode\ text*) r=0;;
      *) r=1;;
    esac
    rm -f $g
    return $r
}

blame_encoding() {
    local f
    f=$1

    cat > $f <<EOF
Unfortunately, the contents of this comment cannot be displayed as it contains
some non-Unicode (UTF-8) characters or it is not a plain ASCII text.  Please
remove those characters from the output of the program that generated it.
EOF
}

force_publish() {
    local src
    local tgt

    src=$1
    tgt=$2

    if [ -s ${src} ] && [ -d $(dirname ${tgt}) ]; then
        mv ${src} ${tgt}
        chown nobody:nogroup ${tgt}
        chmod g+rw,o+rw ${tgt}
    fi
}

publish() {
    check_encoding $1 && force_publish $1 $2 || blame_encoding $2
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
