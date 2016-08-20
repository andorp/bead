#!/bin/bash

/development/init/dev-env-setup.sh
cd /development/bead
stack setup
stack build --flag Bead:Tests --flag Bead:MySQL --flag Bead:LDAP
stack exec BeadTest
