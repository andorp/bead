#!/bin/bash

/development/init/dev-env-setup.sh
cd /development/bead
cabal configure -v2 -f "Tests MySQL SSO"
cabal run BeadTest
