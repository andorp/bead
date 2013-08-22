#! /bin/sh

./dist/build/Test/Test

hpc markup Test --hpcdir=dist/build/Test/Mix --destdir=./dist/hpc

rm Test.tix

