module Bead.View.Snap.Content.Course (
    course
  ) where

import Bead.View.Snap.Content
import Bead.Controller.UserStories (loadCourse)
import Bead.Controller.Pages as P (Page(Group))
import Bead.Domain.Relationships (CourseKey(..))
import Bead.Domain.Types (Str(..))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.ByteString.Char8
import Data.String

course :: Content
course = Content {
    get  = Just coursePage
  , post = Nothing
  }

coursePage :: GETContentHandler
coursePage = withUserStateE $ \s -> do
  key <- getParamE (fieldName courseKeyInfo)
  let courseKey = CourseKey . unpack $ key
  (course, groupKeys) <- runStoryE . loadCourse $ courseKey
  lift $ blaze $ withUserFrame s (courseForm courseKey course groupKeys) Nothing

courseForm :: CourseKey -> Course -> [GroupKey] -> Html
courseForm ck c gks = do
  H.p $ fromString "Course code: " >> (fromString . str . courseCode $ c)
  H.p $ fromString "Course name: " >> (fromString . courseName $ c)
  H.p $ fromString "Course desc: " >> (fromString . courseDesc $ c)
  groupKeys (routeOf P.Group) ck gks

