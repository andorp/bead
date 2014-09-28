{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.GetSubmission (
    getSubmission
  ) where

import           Data.String (fromString)
import qualified Data.ByteString.UTF8 as BsUTF8 (fromString)

import qualified Bead.Controller.UserStories as Story
import           Bead.View.Snap.Content

getSubmission = ViewHandler $ do
  sk <- getParameter submissionKeyPrm
  (s, description) <- userStory (Story.getSubmission sk)
  let submission = solution s
  let basename = concat [usernameCata id $ eUsername description, "_", submissionKeyMap id sk]
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
