{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Course (
    course
  ) where

import Bead.View.Snap.Content
import Bead.View.Snap.RequestParams
import Bead.Controller.UserStories (loadCourse)
import Bead.Controller.Pages as P (Page(Group, CreateGroup))
import Bead.Domain.Relationships (CourseKey(..))
import Bead.Domain.RolePermission (permission)
import Bead.Domain.Types (Str(..))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.ByteString.Char8
import Data.String

course :: Content
course = getContentHandler coursePage

coursePage :: GETContentHandler
coursePage = withUserStateE $ \s -> do
  courseKey <- getParamE (fieldName courseKeyInfo) CourseKey "Course key is not found"
  (course, groupKeys) <- runStoryE . loadCourse $ courseKey
  lift $ blaze $ withUserFrame s (courseForm s courseKey course groupKeys) Nothing

courseForm :: UserState -> CourseKey -> Course -> [GroupKey] -> Html
courseForm s ck c gks = do
  H.p $ fromString "Course code: " >> (fromString . str . courseCode $ c)
  H.p $ fromString "Course name: " >> (fromString . courseName $ c)
  H.p $ fromString "Course desc: " >> (fromString . courseDesc $ c)
  createGroup s ck
  groupKeys (routeOf P.Group) ck gks

createGroup :: UserState -> CourseKey -> Html
createGroup s ck = case permission (role s) P_Create P_Group of
  False -> return ()
  True  -> canCreateGroup ck

canCreateGroup :: CourseKey -> Html
canCreateGroup ck = do
  H.form ! A.method "get" ! A.action (routeWithParams P.CreateGroup [requestParam ck]) $ do
    H.table ! A.id "create-group-table" $ do
      H.tr $ do
        H.td $ H.input ! A.type_ "hidden" ! A.name (fieldName courseKeyInfo) ! A.value (fromString . keyString $ ck)
        H.td $ H.input ! A.type_ "submit" ! A.value "Create Group"


