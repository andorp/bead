{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Utils where

import Bead.View.Snap.Content
import Bead.Controller.UserStories
{-
usersStoryAndAssignment
  :: UserStory a
  -> AssignmentKey
  -> ((Maybe Assignment, a) -> HandlerError App App b)
  -> HandlerError App App b
usersStoryAndAssignment story ak f =
  (runStoryE $ do
     v  <- story
     ks <- userAssignmentKeys
     case elem ak ks of
       False -> return (Nothing,v)
       True  -> do
         a <- loadAssignment ak
         return (Just a,v)
  ) >>= f
-}
{-
usersAssignment
  :: AssignmentKey
  -> (Maybe Assignment -> HandlerError App App b)
  -> HandlerError App App b
usersAssignment ak f =
  (runStoryE $ do
     ks <- userAssignmentKeys
     case elem ak ks of
       False -> return Nothing
       True  -> Just <$> loadAssignment ak
  ) >>= f
-}

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



-- usersAssignment ak f = usersStoryAndAssignment (return ()) ak (\(m,_) -> f m)

