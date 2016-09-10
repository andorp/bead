module Bead.View.Anchor where

import Data.String
import Bead.Domain.Relationships


class Anchor a where
    anchor :: (IsString s) => a -> s

data SubmissionDetailsEvaluationDiv = SubmissionDetailsEvaluationDiv

instance Anchor SubmissionDetailsEvaluationDiv where
    anchor SubmissionDetailsEvaluationDiv = fromString "ev-anchor-div"

instance Anchor CommentKey where
    anchor (CommentKey _k) = fromString $ "cmt-anchor-" ++ "1"

