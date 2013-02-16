{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Pagelets where

import Control.Monad (mapM_)

import Text.Blaze (textTag)
import Text.Blaze.Html5 hiding (base, map, head, menu)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A

import qualified Bead.Controller.Pages as P
import Bead.Controller.ServiceContext (UserState(..))
import Bead.View.Snap.RouteOf

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
    H.div ! A.id "menu" $ navigationMenu s
    H.div ! A.id "content" $ content
    case loggedInContent of
      Nothing -> return ()
      Just inner  -> H.div $ do
        H.div ! A.id "menu1" $ do
          inner


empty :: Html
empty = return ()

errorPage :: Html
errorPage = withUserFrame undefined "Error page is not defined" Nothing

navigationMenu :: UserState -> Html
navigationMenu s =
  mapM_ (\p -> H.p $ H.a ! A.href (routeOf p) $ (name p)) $ P.menuPages (role s) (page s)
  where
    name P.Login      = "Login"
    name P.Home       = "Home"
    name P.Profile    = "Profile"
    name P.Course     = "Course"
    name P.Group      = "Group"
    name P.OpenExam   = "Opened Exam"
    name P.ClosedExam = "Closed Exam"
    name P.Error      = "Error"
    name P.SubmitExam = "Submit Exam"
    name P.Evaulation = "Evaulation"
    name P.Training   = "Training"
    name P.CreateExercise = "Create Exercise"
    name P.Admin      = "Admin"
    name _            = error "navigationMenu"

