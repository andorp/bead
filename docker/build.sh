#!/bin/sh

# Build image
docker --tlsverify=False build --memory=16G -t="andorp/bead" .

