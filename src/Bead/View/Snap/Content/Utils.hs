{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Utils where

import Data.List (find)
import Data.Maybe (maybe)

import Bead.Domain.Relationships (AssignmentDesc)
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

-- Produces a handler that runs the the found handler if the given
-- assignment key is associated with the current user, otherwise
-- runs the notFound handler
userAssignmentForSubmission
  :: AssignmentKey
  -> (AssignmentDesc -> Assignment -> HandlerError App App b)
  -> (HandlerError App App b)
  -> HandlerError App App b
userAssignmentForSubmission key found notFound = do
  action <- runStoryE $ do
    ks <- (maybe [] id) <$> userAssignments
    maybe
      (return notFound)
      foundAssignment
      (findAssignmentKey ks)
  action
  where
    foundAssignment (ak,desc) = (loadAssignment ak) >>= return . found desc
    findAssignmentKey = find (\(k,_v) -> (k==key))

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

