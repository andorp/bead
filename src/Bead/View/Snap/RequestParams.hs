module Bead.View.Snap.RequestParams where

import Data.String (IsString(..))
import Bead.Domain.Relationships
import Bead.View.Snap.RouteOf (ReqParam(..), RequestParam(..), ReqParamValue(..))
import Bead.View.Snap.TemplateAndComponentNames

instance ReqParamValue AssignmentKey where
  paramValue (AssignmentKey a) = fromString a

instance RequestParam AssignmentKey where
  requestParam a = ReqParam (fieldName assignmentKeyField, paramValue a)

instance ReqParamValue SubmissionKey where
  paramValue (SubmissionKey s) = fromString s

instance RequestParam SubmissionKey where
  requestParam s = ReqParam (fieldName submissionKeyField, paramValue s)

instance ReqParamValue GroupKey where
  paramValue (GroupKey g) = fromString g

instance ReqParamValue EvaulationKey where
  paramValue (EvaulationKey e) = fromString e
