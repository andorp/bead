{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.GetSubmission (
    getSubmission
  ) where

import           Data.String (fromString)
import qualified Data.ByteString.UTF8 as BsUTF8 (fromString)

import qualified Bead.Controller.UserStories as Story
import           Bead.View.Snap.Content

getSubmission = ViewHandler $ withUserState $ \s -> do
  sk <- getParameter submissionKeyPrm
  submission <- fmap solution $ userStory (Story.getSubmission sk)
  let basename = concat [usernameCata id $ usernameInState s, "_", submissionKeyMap id sk]
  let fname = submissionValue (const (++ ".txt")) (const (++ ".zip")) submission basename
  modifyResponse $
    setHeader "Content-Disposition" (fromString $ concat ["attachment; filename=\"",fname,"\""])
  submissionValue downloadPlain downloadZipped submission
  where
    downloadPlain text = do
      modifyResponse
        $ setHeader "Content-Type" "text/plain; charset=\"UTF-8\""
      writeBS (BsUTF8.fromString text)

    downloadZipped zip = do
      modifyResponse
        $ setHeader "Content-Type" "application/zip, application/octet-stream"
      writeBS zip
