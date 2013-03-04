module Bead.View.Snap.Content.All (
    content
  , invariants
  ) where

import Bead.Controller.Pages as P hiding (invariants)
import Bead.View.Snap.Content hiding (exercise)
import Bead.View.Snap.Content.Home (home)
import Bead.View.Snap.Content.Admin (admin)
import Bead.View.Snap.Content.CreateExercise (createExercise)
import Bead.View.Snap.Content.Exercise (exercise)
import Bead.View.Snap.Content.Courses (courses)
import Bead.View.Snap.Content.Course (course)

import Bead.Invariants (Invariants(..))

content :: Page -> Content
content P.Login          = emptyContent
content P.Logout         = emptyContent
content P.Home           = home
content P.Profile        = emptyContent
content P.Course         = course
content P.Group          = emptyContent
content P.Groups         = emptyContent
content P.Exercise       = exercise
content P.ClosedExam     = emptyContent
content P.Error          = emptyContent
content P.SubmitExam     = emptyContent
content P.Evaulation     = emptyContent
content P.Training       = emptyContent
content P.Admin          = admin
content P.CreateExercise = createExercise
content P.Courses        = courses

invariants = Invariants [
    ("Content handler must be defined ", \p -> getOrPost p)
  ]
  where
    getOrPost p =
      let c = content p
      in case (get c, post c) of
           (Just _, Just _)  -> True
           (Just _, Nothing) -> True
           (Nothing, Just _) -> True
           (Nothing, Nothing) -> True
