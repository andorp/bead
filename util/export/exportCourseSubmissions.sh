#!/bin/sh

if [ -z "$1" ]; then
  echo "A course identifier must be specified."
  exit 127
fi

_all=0
if [ "$2" = "--all" ]; then
  _all=1
fi

. ./common.sh

exportCourseSubmissions $1 ${_all}
