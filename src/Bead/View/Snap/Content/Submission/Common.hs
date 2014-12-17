{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Submission.Common where

import           Data.String (fromString)

import           Bead.View.Snap.Content

remainingTries msg =
  submissionLimit
    (const $ Nothing)
    (\n _ -> Just (fromString . msg $ Msg_Submission_Remaining "Reamining:", fromString $ show n))
    (const $ Just (fromString . msg $ Msg_Submission_Remaining "Remaining:", fromString . msg $ Msg_Submission_NoTriesLeft "No Tries left."))
