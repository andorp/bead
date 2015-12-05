{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.GetGroupCsv (
    getGroupCsv
  ) where

import           Data.String (fromString)
import qualified Data.ByteString.UTF8 as BsUTF8 (fromString)

import           Bead.View.RequestParams (groupKeyParamName)
import qualified Bead.Controller.UserStories as Story
import           Bead.View.Content
import Control.Monad.IO.Class (liftIO)

getGroupCsv = DataHandler $ do
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  usernames <- userStory (Story.subscribedToGroup gk)
  let fname = groupKeyMap id gk ++ ".csv"
  modifyResponse $
    setHeader "Content-Disposition" (fromString $ concat ["attachment; filename=\"",fname,"\""])
  downloadPlain (csv usernames)
  where
    downloadPlain text = do
      modifyResponse $ setHeader "Content-Type" "text/plain; charset=\"UTF-8\""
      writeBS (BsUTF8.fromString text)

csv :: [Username] -> String
csv users = unlines (header : body)
    where
      header = "Username,Score"
      body = map ((++ ",") . getUserName) users
      getUserName = usernameCata id
