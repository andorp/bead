#!/bin/sh

set -e

cabal clean
cabal configure --flags "SQLite Tests"
cabal install --only-dependencies
cabal test

cabal clean
cabal configure --flags "-LDAP -SQLite Tests"
cabal install --only-dependencies
cabal test
