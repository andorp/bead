#!/bin/sh

# CAUTION: Removes all the images from docker daemon
docker --tlsverify=False rmi -f $(docker --tlsverify=False images -aq)
