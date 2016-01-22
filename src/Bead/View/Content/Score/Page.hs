module Bead.View.Content.Score.Page (
    newUserScore
  , modifyUserScore
  ) where

import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.RequestParams
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story

import           Data.String (fromString)

newUserScore :: ViewModifyHandler
newUserScore = ViewModifyHandler scorePage scorePostHandler

modifyUserScore :: ViewModifyHandler
modifyUserScore = ViewModifyHandler modifyScorePage modifyScorePostHandler

scorePage :: GETContentHandler
scorePage = do 
  username <- getParameter $ usernamePrm
  return (usernameCata fromString username)

scorePostHandler = error "scorePostHandler is undefined"

modifyScorePage = error "modifyScorePage is undefined"

modifyScorePostHandler = error "modifyScorePostHandler is undefined"
