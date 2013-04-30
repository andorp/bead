{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Utils where

import Bead.View.Snap.Content
import Bead.Controller.UserStories

usersObject
  :: (Eq k)
  => UserStory [k]
  -> (k -> UserStory v)
  -> k
  -> (Maybe v -> HandlerError App App a)
  -> HandlerError App App a
usersObject objectKeys keyLoader key onKeyFound =
  (runStoryE $ do
    ks <- objectKeys
    case elem key ks of
      False -> return Nothing
      True  -> Just <$> keyLoader key
  ) >>= onKeyFound

usersAssignment
  :: AssignmentKey
  -> (Maybe Assignment -> HandlerError App App b)
  -> HandlerError App App b
usersAssignment = usersObject userAssignmentKeys loadAssignment

usersSubmission
  :: AssignmentKey
  -> SubmissionKey
  -> (Maybe Submission -> HandlerError App App a)
  -> HandlerError App App a
usersSubmission ak = usersObject (userSubmissionKeys ak) loadSubmission

