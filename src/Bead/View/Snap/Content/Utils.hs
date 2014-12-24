{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Utils where

import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Bead.Controller.UserStories
import Bead.View.Snap.Content

usersObject
  :: (Eq k)
  => UserStory [k]
  -> (k -> UserStory v)
  -> k
  -> (Maybe v -> ContentHandler a)
  -> ContentHandler a
usersObject objectKeys keyLoader key onKeyFound =
  (userStory $ do
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
  -> (AssignmentDesc -> Assignment -> ContentHandler b)
  -> ContentHandler b
  -> ContentHandler b
userAssignmentForSubmission key found notFound = do
  action <- userStory $ do
    ks <- toList <$> userAssignments
    maybe
      (return notFound)
      foundAssignment
      (findAssignmentKey ks)
  action
  where
    foundAssignment (ak,desc,_) = (loadAssignment ak) >>= return . found desc
    findAssignmentKey = find (\(k,_v,_s) -> (k==key))
    toList = Map.fold (++) []

usersAssignment
  :: AssignmentKey
  -> (Maybe Assignment -> ContentHandler b)
  -> ContentHandler b
usersAssignment = usersObject (fmap toList userAssignmentKeys) loadAssignment
  where
    toList = Set.toList . Map.fold Set.union Set.empty

usersSubmission
  :: AssignmentKey
  -> SubmissionKey
  -> (Maybe Submission -> ContentHandler b)
  -> ContentHandler b
usersSubmission ak = usersObject (userSubmissionKeys ak) loadSubmission

