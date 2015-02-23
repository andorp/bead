#!/bin/sh

if [ "$1" = "" ]; then
  echo "A group identifier must be specified."
  exit 127
fi

. ./common.sh

exportSubmissions "group" $1
