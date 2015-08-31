#!/bin/sh

set -e

cabal clean
cabal configure --flags "MySQL"
cabal build

cabal clean
cabal configure --flags "-LDAP -MySQL"
cabal build

cabal clean
cabal configure --flags "MySQL Tests"
cabal install --only-dependencies
cabal test

cabal clean
cabal configure --flags "-LDAP -MySQL Tests"
cabal install --only-dependencies
cabal test
