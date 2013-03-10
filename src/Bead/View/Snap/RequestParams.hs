module Bead.View.Snap.RequestParams where

import Bead.Domain.Relationships (CourseKey(..))
import Bead.View.Snap.RouteOf (ReqParam(..), RequestParam(..))
import Bead.View.Snap.TemplateAndComponentNames

instance RequestParam CourseKey where
  requestParam (CourseKey c) = ReqParam (fieldName courseKeyInfo, c)



