#!/bin/sh

# Check arguments
if [ "$#" -ne 2 ]; then
  echo "Usage rundocker source_dir server_dir"
  exit 1
fi

# Attach volumes and run image
docker --tlsverify=False run --rm -it -p 8000:8000 -v $1:/development/bead -v $2:/bead-server andorp/bead /bin/bash
