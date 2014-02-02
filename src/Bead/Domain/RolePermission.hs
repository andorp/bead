{-# LANGUAGE CPP #-}
module Bead.Domain.RolePermission (
    permission
#ifdef TEST
  , invariants
#endif
  ) where

import Control.Monad (join)
import Bead.Domain.Entities hiding (roles, groupAdmin)
#ifdef TEST
import Bead.Invariants (Invariants(..))
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

student P_Open   o = elem o [P_Comment, P_Assignment, P_Submission, P_Course, P_Group, P_PlainPage]
student P_Create o = elem o [P_Submission, P_Comment]
student P_Modify o = elem o [P_Password]
student P_Delete _ = False

-- * Group Admin

groupAdmin P_Open   o = elem o [P_Assignment, P_Submission, P_Statistics, P_Group, P_PlainPage, P_GroupAdmin]
groupAdmin P_Create o = elem o [P_Assignment, P_Evaluation]
groupAdmin P_Modify o = elem o [P_Assignment, P_Password, P_Evaluation, P_Group]
groupAdmin P_Delete o = elem o [P_Assignment]

-- * Course Admin

courseAdmin P_Open   o = elem o
  [ P_Assignment, P_Submission, P_Statistics, P_Course, P_CourseAdmin, P_PlainPage, P_User ]
courseAdmin P_Create o = elem o [P_Group, P_Assignment, P_CourseAdmin, P_GroupAdmin ]
courseAdmin P_Modify o = elem o [P_Assignment, P_Password, P_CourseAdmin, P_GroupAdmin, P_Course]
courseAdmin P_Delete o = elem o [P_Assignment, P_GroupAdmin]

-- * Admin

admin P_Open   o = elem o [P_Assignment, P_Submission, P_Statistics, P_Course, P_CourseAdmin, P_GroupAdmin, P_AdminPage, P_PlainPage]
admin P_Create o = elem o [P_Course, P_CourseAdmin, P_GroupAdmin, P_Group, P_User]
admin P_Modify o = elem o [P_Course, P_CourseAdmin, P_GroupAdmin, P_Password, P_User]
admin P_Delete o = elem o [P_Course, P_CourseAdmin, P_GroupAdmin]

#ifdef TEST

-- * Invariants

invariants = Invariants [
    ("Permission relation is totally defined",\(r,p,o) -> length (show (permission r p o)) > 0)
  ]
#endif
