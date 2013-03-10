module Bead.Domain.RolePermission (
    permission
  , invariants
  ) where

import Control.Monad (join)
import Bead.Domain.Entities

import Bead.Invariants (Invariants(..))

permission :: Role -> Permission -> PermissionObject -> Bool
permission Student     = student
permission Professor   = professor
permission CourseAdmin = courseAdmin
permission Admin       = admin

student, professor, courseAdmin, admin :: Permission -> PermissionObject -> Bool

-- * Student

student P_Open   o = elem o [P_Exercise, P_Solution, P_Course, P_PlainPage]
student P_Create o = elem o [P_Solution]
student P_Modify o = elem o [P_Password]
student P_Delete _ = False

-- * Professor

professor P_Open   o = elem o [P_Exercise, P_Solution, P_Statistics, P_Course, P_PlainPage, P_Professor]
professor P_Create o = elem o [P_Exercise]
professor P_Modify o = elem o [P_Exercise, P_Password]
professor P_Delete o = elem o [P_Exercise]

-- * Course Admin

courseAdmin P_Open   o = elem o
  [ P_Exercise, P_Solution, P_Statistics, P_Course, P_CourseAdmin, P_PlainPage ]
courseAdmin P_Create o = elem o [P_Exercise, P_CourseAdmin, P_Professor ]
courseAdmin P_Modify o = elem o [P_Exercise, P_Password, P_CourseAdmin, P_Professor ]
courseAdmin P_Delete o = elem o [P_Exercise, P_Professor]

-- * Admin

admin P_Open   o = elem o [P_Exercise, P_Solution, P_Statistics, P_Course, P_CourseAdmin, P_Professor, P_AdminPage, P_PlainPage]
admin P_Create o = elem o [P_Course, P_CourseAdmin, P_Professor, P_Group]
admin P_Modify o = elem o [P_Course, P_CourseAdmin, P_Professor, P_Password]
admin P_Delete o = elem o [P_Course, P_CourseAdmin, P_Professor]

-- * Invariants

invariants = Invariants [
    ("Permission relation is totally defined",\(r,p,o) -> length (show (permission r p o)) > 0)
  ]
