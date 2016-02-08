{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.GetCsv (
    getGroupCsv 
  , getCourseCsv
  ) where

import           Bead.View.RequestParams (groupKeyParamName,courseKeyParamName)
import qualified Bead.Controller.UserStories as Story
import           Bead.View.Content
import           Bead.View.Content.ScoreInfo (scoreInfoToRawText)

import           Control.Monad (forM)
import           Control.Monad.Trans (lift)
import qualified Data.ByteString.UTF8 as BsUTF8 (fromString)
import           Data.Function (on)
import           Data.String (fromString)
import           Data.List (sortBy,intercalate)
import           System.FilePath (FilePath)

getGroupCsv :: DataHandler
getGroupCsv = DataHandler $ do
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  maybeAk  <- getOptionalParameter assessmentKeyPrm
  msg <- lift i18nH
  let filename = groupKeyMap id gk ++ ".csv"
  case maybeAk of
    Just ak -> do
      let filename = concat [groupKeyMap id gk, "_", assessmentKey id ak, ".csv"]
      groupScores <- userStory (Story.scoresOfGroup gk ak)
      downloadFile filename (csvFilled msg groupScores)
    Nothing -> do 
      let filename = concat [groupKeyMap id gk,".csv"]
      users <- userStory $ do
        usernames <- Story.subscribedToGroup gk
        mapM Story.loadUserDesc usernames
      downloadFile filename (csvEmpty msg users)

getCourseCsv :: DataHandler
getCourseCsv = DataHandler $ do
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  maybeAk  <- getOptionalParameter assessmentKeyPrm
  msg <- lift i18nH
  case maybeAk of
    Just ak -> do
      let filename = concat [courseKeyMap id ck, "_", assessmentKey id ak, ".csv"]
      courseScores <- userStory (Story.scoresOfCourse ck ak)      
      downloadFile filename (csvFilled msg courseScores)
    Nothing -> do
      let filename = concat [courseKeyMap id ck,".csv"]
      users <- userStory $ do
        usernames <- Story.subscribedToCourse ck
        mapM Story.loadUserDesc usernames
      downloadFile filename (csvEmpty msg users)

downloadFile :: FilePath -> String -> ContentHandler ()
downloadFile filename content = do
  modifyResponse $
    addHeader "Content-Disposition" (fromString . concat $ ["attachment; filename=\"",filename,"\""])
  downloadPlain
  where
    downloadPlain = do
      modifyResponse $ setContentType "text/plain; charset=\"UTF-8\""
      writeBS (BsUTF8.fromString content)

csvEmpty :: I18N -> [UserDesc] -> String
csvEmpty msg users = (information msg) ++ unlines (header : body)
    where
      header = intercalate "," [name,username,score]
          where
            name = msg . msg_GetCsv_StudentName $ "Name"
            username = msg . msg_GetCsv_Username $ "Username"
            score = msg . msg_GetCsv_Score $ "Score"

      body = map line (sortBy (compare `on` fullName) users)

      line user = intercalate "," [fullName user, userid user, ""]
      userid = uid id . ud_uid 
      fullName = ud_fullname

csvFilled :: I18N -> [(UserDesc, Maybe ScoreInfo)] -> String
csvFilled msg users = (information msg) ++ unlines (header : body)
    where
      header = intercalate "," [name,username,score]
          where
            name = msg . msg_GetCsv_StudentName $ "Name"
            username = msg . msg_GetCsv_Username $ "Username"
            score = msg . msg_GetCsv_Score $ "Score"

      body = map line (sortBy (compare `on` (fullName . fst)) users)

      line (user,mScoreInfo) = intercalate "," [fullName user, userid user, score]
          where score = case mScoreInfo of
                          Nothing        -> ""
                          Just scoreInfo -> scoreInfoToRawText "" msg scoreInfo
      userid = uid id . ud_uid 
      fullName = ud_fullname

information :: I18N -> String
information msg = msg . msg_GetCsv_Information $ unlines
              [ "# Lines starting with '#' will be ignored."
              , "# The following scores are valid:"
              , "#  - In case of binary evaluation:"
              , "#      Accepted may be written as '+', '1' or 'Accepted'."
              , "#      Rejected may be written as '-', '0' or 'Rejected'."
              , "#      Interpretation is case in-sensitive."
              , "#  - In case of percentage evaluation: an integer from 0 to 100, inclusive"
              , "#  - In case of free form evaluation: text ending with newline character."
              ]
