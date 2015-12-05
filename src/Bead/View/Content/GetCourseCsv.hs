{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.GetCourseCsv (
    getCourseCsv
  ) where

import           Data.String (fromString)
import qualified Data.ByteString.UTF8 as BsUTF8 (fromString)

import           Bead.View.RequestParams (courseKeyParamName)
import qualified Bead.Controller.UserStories as Story
import           Bead.View.Content

getCourseCsv = DataHandler $ do
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  usernames <- userStory (Story.subscribedToCourse ck)
  let fname = courseKeyMap id ck ++ ".csv"
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
