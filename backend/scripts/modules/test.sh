#!/bin/sh

usage() {
    echo "bead test [jailname] [job_id]

Parameters:
    jailname     -- Jail to use
    job_id       -- Job ID to test
"
    exit 1
}

JAILNAME="$1"
JOB_ID="$2"

test -z "${JAILNAME}" && usage
test -z "${JOB_ID}"   && usage

SCRIPT_PATH=`realpath $0`
SCRIPT_PREFIX=`dirname ${SCRIPT_PATH}`

. ${SCRIPT_PREFIX}/common.sh

_INPUT="${SCRIPT_PREFIX}/../jobs/${JAILNAME}/incoming/${JOB_ID}"

if [ ! -d "${_INPUT}" ]; then
    msg "Job ${JOB_ID} cannot be found."
    exit 1
fi

JAIL_PATH="${SCRIPT_PREFIX}/../jails/${JAILNAME}"

if [ ! -d "${JAIL_PATH}" ]; then
    msg "Jail ${JAILNAME} cannot be found."
    exit 1
fi

INPUT="${_INPUT}${PENDING}"

# grab job
mv ${_INPUT} ${INPUT}

OUTPUT_DIR="${SCRIPT_PREFIX}/../jobs/${JAILNAME}/outgoing/${JOB_ID}"
OUTPUT="${OUTPUT_DIR}/private"
MESSAGE="${OUTPUT_DIR}/public"
RESULT="${OUTPUT_DIR}/result"
TEMPLATES="${SCRIPT_PREFIX}/../templates"
BUILD_PATH="/bead/build"
SANDBOX_PATH="/bead/run"

. ${INPUT}/script

publish() {
    local src
    local tgt

    src=$1
    tgt=$2

    if [ -s ${src} ]; then
        mv ${src} ${tgt}
        chmod g+w,o+w ${tgt}
    fi
}

test_build() {
    local build_result
    local result
    local build_log
    local build_msg
    local build_rst

    build_log=/tmp/build.${JOB_ID}.log
    build_msg=${JAIL_PATH}${BUILD_PATH}/.message
    build_rst=${JAIL_PATH}${BUILD_PATH}/.result

    mkdir -p ${JAIL_PATH}${BUILD_PATH}
    mkdir -p ${JAIL_PATH}${SANDBOX_PATH}
    cp ${INPUT}/submission ${INPUT}/tests ${INPUT}/script ${TEMPLATES}/build.sh ${JAIL_PATH}${BUILD_PATH}
    chown -R nobody:nogroup ${JAIL_PATH}${BUILD_PATH}
    chown -R nobody:nogroup ${JAIL_PATH}${SANDBOX_PATH}
    chroot -u nobody -g nogroup ${JAIL_PATH} ${BUILD_PATH}/build.sh ${SANDBOX_PATH} > ${build_log} 2>&1
    build_result="$?"
    mkdir -p ${OUTPUT_DIR}
    chmod g+w,o+w ${OUTPUT_DIR}
    if [ "${build_result}" -ne "0" ]; then
        publish ${build_log} ${OUTPUT}
        publish ${build_msg} ${MESSAGE}
        echo "False" > ${build_rst}
        publish ${build_rst} ${RESULT}
        rm -rf ${JAIL_PATH}${SANDBOX_PATH}
        result=1
    else
        rm -f ${build_log}
        result=0
    fi
    rm -rf ${JAIL_PATH}${BUILD_PATH}
    return ${result}
}

test_run() {
    local run_result
    local run_log
    local run_msg
    local run_rst

    run_log=/tmp/run.${JOB_ID}.log
    run_msg=${JAIL_PATH}${SANDBOX_PATH}/.message
    run_rst=${JAIL_PATH}${SANDBOX_PATH}/.result

    cp ${INPUT}/script ${INPUT}/tests ${TEMPLATES}/run.sh ${JAIL_PATH}${SANDBOX_PATH}
    cd ${JAIL_PATH}${SANDBOX_PATH}
    chown -R nobody:nogroup ${JAIL_PATH}${SANDBOX_PATH}
    chroot -u nobody -g nogroup ${JAIL_PATH} ${SANDBOX_PATH}/run.sh > ${run_log} 2>&1
    run_result="$?"

    if [ "${run_result}" -ne "0" ]; then echo "False" > ${run_rst}
    else echo "True" > ${run_rst}; fi

    publish ${run_log} ${OUTPUT}
    publish ${run_msg} ${MESSAGE}
    publish ${run_rst} ${RESULT}
    rm -rf ${JAIL_PATH}${SANDBOX_PATH}
    return ${run_result}
}

msg_n "[${JOB_ID}] Building ($$)..."

# Now you have ${WATCHDOG_TIMEOUT} seconds to run (at maximum).
/bin/sh ${SCRIPT_PREFIX}/watchdog.sh $$ ${OUTPUT} &

test_build
build_result="$?"
echo "result=${build_result}"

if [ "${build_result}" -eq "0" ]; then
    msg_n "[${JOB_ID}] Running ($$)..."
    test_run
    run_result="$?"
    echo "result=${run_result}"
fi

# XXX: remove directory
# rm -rf ${INPUT}
