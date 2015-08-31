#!/bin/sh

# GHC package cosmetics
ghc-pkg expose fay-text
ghc-pkg hide regex-tdfa-rc

# Start MySQL and create database
service mysql start
mysqladmin -u root password 'password'
echo "create database bead;" | mysql -u root -ppassword
