{-# LANGUAGE CPP #-}
module Bead.Domain.RolePermission (
    permission
#ifdef TEST
  , permissionTest
#endif
  ) where

import Bead.Domain.Entities hiding (roles, groupAdmin)
#ifdef TEST
import Control.Applicative
import Test.Tasty.Arbitrary
import Test.Tasty.TestSet
#endif

permission :: Role -> Permission -> PermissionObject -> Bool
permission Student     = student
permission GroupAdmin  = roles [groupAdmin, student]
permission CourseAdmin = roles [courseAdmin, groupAdmin, student]
permission Admin       = roles [admin, courseAdmin, groupAdmin, student]

student, groupAdmin, courseAdmin, admin :: Permission -> PermissionObject -> Bool

type Perm = Permission -> PermissionObject -> Bool

roles :: [Perm] -> Perm
roles ps p o = or $ map (\f -> f p o) ps

-- * Student

student P_Open   o = elem o [P_Comment, P_Assignment, P_Assessment, P_Submission, P_Course, P_Group, P_PlainPage]
student P_Create o = elem o [P_Submission, P_Comment]
student P_Modify o = elem o [P_Password]
student P_Delete _ = False

-- * Group Admin

groupAdmin P_Open   o = elem o [P_Assignment, P_Assessment, P_Submission, P_Statistics, P_Group, P_PlainPage, P_GroupAdmin, P_TestScript, P_File]
groupAdmin P_Create o = elem o [P_Assignment, P_Assessment, P_Evaluation, P_File]
groupAdmin P_Modify o = elem o [P_Assignment, P_Assessment, P_Password, P_Evaluation, P_Group, P_StudentPassword]
groupAdmin P_Delete o = elem o [P_Assignment, P_Assessment]

-- * Course Admin

courseAdmin P_Open   o = elem o
  [ P_Assignment, P_Assessment, P_Submission, P_Statistics, P_Course, P_CourseAdmin, P_PlainPage, P_User, P_TestScript, P_File ]
courseAdmin P_Create o = elem o [P_Group, P_Assignment, P_Assessment, P_CourseAdmin, P_GroupAdmin, P_TestScript, P_File ]
courseAdmin P_Modify o = elem o [P_Assignment, P_Assessment, P_Password, P_CourseAdmin, P_GroupAdmin, P_Course, P_TestScript, P_StudentPassword]
courseAdmin P_Delete o = elem o [P_Assignment, P_Assessment, P_GroupAdmin]

-- * Admin

admin P_Open   o = elem o [P_Assignment, P_Assessment, P_Submission, P_Statistics, P_Course, P_CourseAdmin, P_GroupAdmin, P_AdminPage, P_PlainPage, P_File]
admin P_Create o = elem o [P_Course, P_CourseAdmin, P_GroupAdmin, P_Group, P_User, P_File]
admin P_Modify o = elem o [P_Course, P_CourseAdmin, P_GroupAdmin, P_Password, P_User]
admin P_Delete o = elem o [P_Course, P_CourseAdmin, P_GroupAdmin]

#ifdef TEST

permissionTest =
  assertProperty
    "Permission is a total function"
    (\(r,p,o) -> length (show (permission r p o)) > 0)
    ((,,) <$> enumGen <*> enumGen <*> enumGen)
    "Permission relation is not totally defined"

#endif
