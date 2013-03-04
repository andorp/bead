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
coursePage = withUserState $ \s -> do
  mKey <- getParam (fieldName courseKeyInfo)
  case mKey of
    Nothing -> error "Bead.View.Snap.Content.Course.coursePage"
    Just key -> do
      cs <- runStory . loadCourse . CourseKey . unpack $ key
      case cs of
        Left err -> error "Error happened: loading course"
        Right (cs',gks) -> do
          blaze $ withUserFrame s (courseForm cs' gks) Nothing

courseForm :: Course -> [GroupKey] -> Html
courseForm c gks = do
  H.p $ fromString "Course code: " >> (fromString . str . courseCode $ c)
  H.p $ fromString "Course name: " >> (fromString . courseName $ c)
  H.p $ fromString "Course desc: " >> (fromString . courseDesc $ c)
  groupKeys (routeOf P.Group) gks

