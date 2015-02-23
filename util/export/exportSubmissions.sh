#!/bin/sh

if [ "$1" = "" ]; then
  echo "An assignment identifier must be specified."
  exit 127
fi

. ./common.sh

exportSubmissions1 $1
