{-# LANGUAGE OverloadedStrings #-}
module Bead.Persistence.SQL.TestData where

import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Domain.Shared.Evaluation

course  = Course "name" "desc" TestScriptSimple

group   = Group "name" "desc"

time    = read "2014-06-09 12:55:27 UTC"

sbm     = Submission (SimpleSubmission "submission") time
sbm2    = Submission (ZippedSubmission "submission2") time

ballot  = aspectsFromList [BallotBox]
normal  = aspectsFromList []

asg     = Assignment "name" "desc" ballot time time binaryConfig
asg2    = Assignment "name2" "desc2" normal time time (percentageConfig 0.1)

ast     = Assessment "title" "this is an assessment" time binaryConfig
ast2    = Assessment "title2" "this is an assessment 2" time (percentageConfig 0.1)

user1name = Username "USER1"
user2name = Username "USER2"

user1 = User Student user1name (Email "email2") "name2" (TimeZoneName "UTC") (Language "hu") (Uid "USR01")
user2  = User Student user2name (Email "email2") "name2" (TimeZoneName "UTC") (Language "es") (Uid "USR02")

ev    = Evaluation (binaryResult Passed) "written"
ev2   = Evaluation (percentageResult 0.01) "escrito"

cmt   = Comment "comment" "User" time CT_Student

scr   = Score ()

reg = UserRegistration "username" "email" "name" "token" time

script  = TestScript "name" "desc" "notes" "script" TestScriptSimple
script2 = TestScript "name2" "desc2" "notes2" "script2" TestScriptZipped

case1   = TestCase "name" "desc" (SimpleTestCase "blah") "info"
case2   = TestCase "name2" "desc2" (ZippedTestCase "zipped") "info"

fbTestResult = Feedback (TestResult True) time
fbMsgStudent  = Feedback (MessageForStudent "student") time
fbMsgForAdmin = Feedback (MessageForAdmin "admin") time
fbEvaluated = Feedback (Evaluated (percentageResult 0.1) "eval" "author") time
