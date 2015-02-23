: ${DBROOT:=data}
: ${EXPORTDIR:=export}

exportFile() {
  local _src
  local _tgt

  _src="$1"
  _tgt="$2"

  mkdir -p $(dirname "${_tgt}")
  cp -p "${_src}" "${_tgt}"
}

exportAssignments() {
  local _type
  local _id
  local _assignments
  local _name

  _type=$1
  _id=$2
  _assignments=${DBROOT}/${_type}/${_id}/assignments
  if [ ! -d "${_assignments}" ]; then
    echo "ERROR: ${_id} does not exist as ${_type}."
    return 127
  fi
  for a in $(ls ${_assignments}); do
    echo -n "Exporting ${_assignments}/$a (${_type})..."
    _name=$(cat ${_assignments}/$a/name)
    exportFile "${_assignments}/$a/description" "${EXPORTDIR}/${_id}/$a.${_name}.txt"
    echo "OK"
  done
}

exportCourseAssignments() {
  local _id
  local _all
  local _groups

  _id=$1
  _all=$2
  exportAssignments "course" ${_id}
  if [ ${_all} -ne 0 ]; then
    _groups=${DBROOT}/course/${_id}/groups
    if [ ! -d "${_groups}" ]; then
      echo "ERROR: ${_id} does not have groups."
      return 127
    fi
    for g in $(ls ${_groups}); do
      exportAssignments "group" $g
    done
  fi
}

exportSubmissions1() {
  local _id
  local _submissions
  local _type
  local _name
  local _user
  local _src
  local _tgt

  _id=$1
  _submissions=${DBROOT}/assignment/${_id}/submission
  if [ ! -d "${_submissions}" ]; then
    echo "ERROR: ${_id} does not have submissions."
    return 127
  fi
  for s in $(ls ${_submissions}); do
    _type=$(cat ${_submissions}/$s/type)
    _name=$(cat ${_submissions}/$s/user/*/name)
    _user=$(cat ${_submissions}/$s/user/*/username)
    _src="${_submissions}/$s/solution"
    _tgt="${EXPORTDIR}/${_id}/$s.${_name} (${_user})"
    echo -n "Exporting ${_submissions}/$s (${_type})..."
    case "${_type}" in
      "simple") exportFile "${_src}" "${_tgt}.txt" ;;
      "zipped") exportFile "${_src}" "${_tgt}.zip" ;;
      *) echo "WARNING: submission type is unknown, hence NOT " ;;
    esac
    echo "OK"
  done
}

exportSubmissions() {
  local _type
  local _id

  _type=$1
  _id=$2
  _assignments=${DBROOT}/${_type}/${_id}/assignments
  if [ ! -d "${_assignments}" ]; then
    echo "ERROR: ${_id} does not have assignments."
    return 127
  fi
  for a in $(ls ${_assignments}); do
    exportSubmissions1 $a
  done
}

exportCourseSubmissions() {
  local _id
  local _all
  local _export

  _id=$1
  _all=$2
  _export=${EXPORTDIR}
  EXPORTDIR=${_export}/${_id}
  exportSubmissions "course" ${_id}
  if [ ${_all} -ne 0 ]; then
    _groups=${DBROOT}/course/${_id}/groups
    if [ ! -d "${_groups}" ]; then
      echo "ERROR: ${_id} does not have groups."
      return 127
    fi
    for g in $(ls ${_groups}); do
      EXPORTDIR=${_export}/$g
      exportSubmissions "group" $g
    done
  fi
  EXPORTDIR=${_export}
}
