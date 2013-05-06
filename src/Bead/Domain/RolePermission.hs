module Bead.Domain.RolePermission (
    permission
  , invariants
  ) where

import Control.Monad (join)
import Bead.Domain.Entities hiding (roles)

import Bead.Invariants (Invariants(..))

permission :: Role -> Permission -> PermissionObject -> Bool
permission Student     = student
permission Professor   = roles [professor, student]
permission CourseAdmin = roles [courseAdmin, professor, student]
permission Admin       = roles [admin, courseAdmin, professor, student]

student, professor, courseAdmin, admin :: Permission -> PermissionObject -> Bool

type Perm = Permission -> PermissionObject -> Bool

roles :: [Perm] -> Perm
roles ps p o = or $ map (\f -> f p o) ps

-- * Student

student P_Open   o = elem o [P_Comment, P_Assignment, P_Submission, P_Course, P_Group, P_PlainPage]
student P_Create o = elem o [P_Submission, P_Comment]
student P_Modify o = elem o [P_Password]
student P_Delete _ = False

-- * Professor

professor P_Open   o = elem o [P_Assignment, P_Submission, P_Statistics, P_Group, P_PlainPage, P_Professor]
professor P_Create o = elem o [P_Assignment, P_Evaulation]
professor P_Modify o = elem o [P_Assignment, P_Password, P_Evaulation]
professor P_Delete o = elem o [P_Assignment]

-- * Course Admin

courseAdmin P_Open   o = elem o
  [ P_Assignment, P_Submission, P_Statistics, P_Course, P_CourseAdmin, P_PlainPage, P_User ]
courseAdmin P_Create o = elem o [P_Group, P_Assignment, P_CourseAdmin, P_Professor ]
courseAdmin P_Modify o = elem o [P_Assignment, P_Password, P_CourseAdmin, P_Professor ]
courseAdmin P_Delete o = elem o [P_Assignment, P_Professor]

-- * Admin

admin P_Open   o = elem o [P_Assignment, P_Submission, P_Statistics, P_Course, P_CourseAdmin, P_Professor, P_AdminPage, P_PlainPage]
admin P_Create o = elem o [P_Course, P_CourseAdmin, P_Professor, P_Group, P_User]
admin P_Modify o = elem o [P_Course, P_CourseAdmin, P_Professor, P_Password, P_User]
admin P_Delete o = elem o [P_Course, P_CourseAdmin, P_Professor]

-- * Invariants

invariants = Invariants [
    ("Permission relation is totally defined",\(r,p,o) -> length (show (permission r p o)) > 0)
  ]
