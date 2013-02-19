{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Pagelets where

import Data.String (IsString(..), fromString)
import Control.Monad (mapM_)

import Text.Blaze (textTag)
import Text.Blaze.Html5 hiding (base, map, head, menu)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A

import Bead.Domain.Types (Str(..))
import Bead.Domain.Relationships (ExerciseKey(..))
import qualified Bead.Controller.Pages as P
import Bead.Controller.ServiceContext (UserState(..))
import Bead.View.Snap.RouteOf
import Bead.View.Snap.TemplateAndComponentNames

-- Definitions --

class BlazeTemplate b where
  template :: b -> Html

withTitleAndHead :: Html -> Html
withTitleAndHead content = docTypeHtml $ do
  H.head $ title "Snap web server"
  body $ do
    H.div ! A.id "content" $ content

withUserFrame :: UserState -> Html -> Maybe Html -> Html
withUserFrame s content loggedInContent = docTypeHtml $ do
  H.head $ title "Snap web server"
  body $ do
    H.div ! A.id "header" $ pageHeader s
    H.div ! A.id "menu" $ navigationMenu s
    H.div ! A.id "content" $ do
      "Content"
      content
    case loggedInContent of
      Nothing -> return ()
      Just inner  -> H.div $ do
        H.div ! A.id "menu1" $ do
          inner

empty :: Html
empty = return ()

errorPage :: Html
errorPage = withUserFrame undefined "Error page is not defined" Nothing

linkText :: (IsString s) => P.Page -> s
linkText P.Login      = fromString "Login"
linkText P.Logout     = fromString "Logout"
linkText P.Home       = fromString "Home"
linkText P.Profile    = fromString "Profile"
linkText P.Course     = fromString "Course"
linkText P.Group      = fromString "Group"
linkText P.Exercise   = fromString "Exercise"
linkText P.ClosedExam = fromString "Closed Exam"
linkText P.Error      = fromString "Error"
linkText P.SubmitExam = fromString "Submit Exam"
linkText P.Evaulation = fromString "Evaulation"
linkText P.Training   = fromString "Training"
linkText P.CreateExercise = fromString "Create Exercise"
linkText P.Admin      = fromString "Admin"

linkToPage :: P.Page -> Html
linkToPage g = H.p $ H.a ! A.href (routeOf g) $ linkText g

navigationMenu :: UserState -> Html
navigationMenu s = do
  H.p $ "Menu"
  mapM_ (linkToPage) $ P.menuPages (role s) (page s)
  H.p $ "Menu"

exerciseKeys :: AttributeValue -> [ExerciseKey] -> Html
exerciseKeys act es = mapM_ exercise es where
  exercise (ExerciseKey e) =
    H.div ! A.id "exercise" $ do
      "Exercises for the user"
      H.form ! A.method "get" ! A.action act $ do
        H.table ! A.id "exercise-table" $ do
          H.tr $ do
            H.td $ H.input ! A.type_ "hidden" ! A.name (fieldName exerciseKey) ! A.value (fromString e)
            H.td $ H.input ! A.type_ "submit" ! A.value (fromString e)

pageHeader :: UserState -> Html
pageHeader s = do
  H.p $ "Header"
  H.span $ do { "Welcome "; fromString . str . user $ s ; "!"  }
  linkToPage P.Logout
  H.p $ "Header"
