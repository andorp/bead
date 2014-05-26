#! /bin/sh

./dist/build/BeadUnitTest/BeadUnitTest

hpc markup Test --hpcdir=dist/build/BeadUnitTest/Mix --destdir=./dist/hpc

rm BeadUnitTest.tix

