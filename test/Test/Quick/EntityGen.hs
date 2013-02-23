module Bead.Invariants.EntityGen where

import Bead.Domain.Entities

import Test.QuickCheck.Gen
import Control.Monad (join, liftM)
import Data.List (nub)

word = listOf1 $ elements ['a' .. 'z' ]
numbers = listOf1 $ elements ['0' .. '9']

manyWords = do
  w <- word
  ws <- manyWords'
  return $ w ++ ws

  where
    manyWords' = listOf1 $ elements $ ' ':['a' .. 'z']


usernames = liftM Username word

roles = elements [Student, Professor, CourseAdmin, Admin]

emails = do
  user <- word
  domain <- word
  return $ Email $ join [user, "@", domain, ".com"]

familyNames = do
  first <- word
  last <- word
  return $ join [first, " ", last]

users = do
  username <- usernames
  role <- roles
  email <- emails
  familyName <- familyNames
  return $ User {
      u_role = role
    , u_username = username
    , u_email = email
    , u_name  = familyName
    }

userAndEPwds = do
  user <- users
  code <- numbers
  return (user, code)

courseCodes = liftM CourseCode word

courseNames = word

courseDescs = manyWords

courses = do
  code <- courseCodes
  name <- courseNames
  desc <- courseDescs
  return $ Course {
      courseCode = code
    , courseName = name
    , courseDesc = desc
    }

groupCodes = word

groupNames = manyWords

groupDescs = manyWords

groupUsers' = liftM (map username) (listOf1 word)

groups = do
  code <- groupCodes
  name <- groupNames
  desc <- groupDescs
  usrs <- groupUsers'
  return $ Group {
      groupCode  = code
    , groupName  = name
    , groupDesc  = desc
    , groupUsers = nub $ usrs
    }
