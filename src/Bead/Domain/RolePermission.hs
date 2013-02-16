module Bead.Domain.RolePermission (
    permission
  ) where

import Control.Monad (join)
import Bead.Domain.Entities

permission :: Role -> Permission -> PermissionObject -> Bool
permission Student     = student
permission Professor   = professor
permission CourseAdmin = courseAdmin
permission Admin       = admin
permission r           = error $ "permission is not defined for the role: " ++ show r

student, professor, courseAdmin, admin :: Permission -> PermissionObject -> Bool

-- * Student

student P_Open   o = elem o [P_Exercise, P_Solution, P_Course, P_PlainPage]
student P_Create o = elem o [P_Solution]
student P_Modify o = elem o [P_Password]
student P_Delete _ = False
student p        o = error $ join [
    "No Student rule was defined for ", show p, " ", show o
  ]

-- * Professor

professor P_Open   o = elem o [P_Exercise, P_Solution, P_Statistics, P_Course, P_PlainPage, P_Professor]
professor P_Create o = elem o [P_Exercise]
professor P_Modify o = elem o [P_Exercise, P_Password]
professor P_Delete o = elem o [P_Exercise]
professor p        o = error $ join [
    "No Professor rule was defined for ", show p, " ", show o
  ]

-- * Course Admin

courseAdmin P_Open   o = elem o
  [ P_Exercise, P_Solution, P_Statistics, P_Course, P_CourseAdmin, P_PlainPage ]
courseAdmin P_Create o = elem o [P_Exercise, P_CourseAdmin, P_Professor ]
courseAdmin P_Modify o = elem o [P_Exercise, P_Password, P_CourseAdmin, P_Professor ]
courseAdmin P_Delete o = elem o [P_Exercise, P_Professor]
courseAdmin p        o = error $ join [
    "No Professor rule was defined for ", show p, " ", show o
  ]

-- * Admin

admin P_Open   o = elem o [P_Exercise, P_Solution, P_Statistics, P_Course, P_CourseAdmin, P_Professor, P_AdminPage, P_PlainPage]
admin P_Create o = elem o [P_Course, P_CourseAdmin, P_Professor]
admin P_Modify o = elem o [P_Course, P_CourseAdmin, P_Professor, P_Password]
admin P_Delete o = elem o [P_Course, P_CourseAdmin, P_Professor]
admin p       o = error $ join [
    "No Professor rule was defined for ", show p, " ", show o
  ]

