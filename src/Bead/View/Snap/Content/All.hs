module Bead.View.Snap.Content.All (
    content
  ) where

import Bead.Controller.Pages as P
import Bead.View.Snap.Content hiding (exercise)
import Bead.View.Snap.Content.Home (home)
import Bead.View.Snap.Content.Admin (admin)
import Bead.View.Snap.Content.CreateExercise (createExercise)
import Bead.View.Snap.Content.Exercise (exercise)

content :: Page -> Content
content P.Login          = emptyContent
content P.Home           = home
content P.Profile        = emptyContent
content P.Course         = emptyContent
content P.Group          = emptyContent
content P.Exercise       = exercise
content P.ClosedExam     = emptyContent
content P.Error          = emptyContent
content P.SubmitExam     = emptyContent
content P.Evaulation     = emptyContent
content P.Training       = emptyContent
content P.Admin          = admin
content P.CreateExercise = createExercise
content p              = error $ "Content Area was not defined for page: " ++ (show p)
