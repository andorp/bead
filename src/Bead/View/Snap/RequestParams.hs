module Bead.View.Snap.RequestParams where

import Data.String (IsString(..))
import Bead.Domain.Relationships
import Bead.View.Snap.RouteOf (ReqParam(..), RequestParam(..), ReqParamValue(..))
import Bead.View.Snap.TemplateAndComponentNames

instance ReqParamValue AssignmentKey where
  paramValue (AssignmentKey a) = fromString a

instance RequestParam AssignmentKey where
  requestParam a = ReqParam (fieldName assignmentKeyField, paramValue a)

instance ReqParamValue GroupKey where
  paramValue (GroupKey g) = fromString g
