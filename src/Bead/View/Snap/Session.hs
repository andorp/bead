{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
module Bead.View.Snap.Session where

-- Bead imports

import Bead.Domain.Entities as E
import Bead.Domain.Relationships as R
import qualified Bead.Controller.Pages as P
import Bead.View.Snap.Application
import Bead.View.Snap.Dictionary (Language(..))
#ifdef TEST
import Bead.Invariants (Invariants(..), UnitTests(..))
#endif

-- Haskell imports

import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.ByteString.Char8 hiding (index, length)
import qualified Data.Text as T
import qualified Data.List as L

import Snap hiding (get)
import Snap.Snaplet.Auth as A
import Snap.Snaplet.Session

-- * Session Management

class SessionStore s where
  sessionStore :: s -> [(T.Text, T.Text)]

class SessionRestore s where
  restoreFromSession :: [(T.Text, T.Text)] -> Maybe s

-- * Public and private session information

-- Produces True if the given key represents a private
-- information in the session
isPrivateKey :: T.Text -> Bool
isPrivateKey t
  | t == pageSessionKey = True
  | t == usernameSessionKey = True
  | otherwise = False

-- Produces False if the given key represens a public
-- information in the session
isPublicKey :: T.Text -> Bool
isPublicKey t
  | t == sessionVersionKey = True
  | t == languageSessionKey = True
  | otherwise = False

-- Removes the session values from the session that satisties the key predicate
removeSessionKeys :: (T.Text -> Bool) -> Handler App b ()
removeSessionKeys pred = withTop sessionManager $ do
  values <- L.filter (not . pred . fst) <$> sessionToList
  resetSession
  setInSessionKeyValues values
  commitSession

-- Clears the private session data from the session
resetPrivateSessionData :: Handler App b ()
resetPrivateSessionData = removeSessionKeys isPrivateKey

-- Clears the public session data from the session
resetPublicSessionData :: Handler App b ()
resetPublicSessionData = removeSessionKeys isPublicKey

-- * Session Key and Values for Page

pageSessionKey :: T.Text
pageSessionKey = "Page"

instance SessionStore P.Page where
  sessionStore p = [(pageSessionKey, T.pack $ s p)] where
    s = P.pageCata
          "Login"
          "Logout"
          "Home"
          "Profile"
          "Error"
          "Administration"
          "CourseAdmin"
          "EvaluationTable"
          (\(R.SubmissionKey s) -> join ["Evaluation:",s])
          (\(R.SubmissionKey s) (R.EvaluationKey e) -> join ["ModifyEvaluation:", s, ":", e])
          "NewGroupAssignment"
          "NewCourseAssignment"
          "ModifyAssignment"
          "Submission"
          "SubmissionList"
          (\(R.AssignmentKey a) (R.SubmissionKey s) -> join ["SubmissionDetails:", a, ":", s])
          "GroupRegistration"
          "UserDetails"
          "UserSubmissions"
          "CreateCourse"
          "CreateGroup"
          "AssignCourseAdmin"
          "AssignGroupAdmin"
          "ChangePassword"
          "SetUserPassword"
          (\(R.SubmissionKey s) -> join ["CommentFromEvaluation:", s])
          (\(R.SubmissionKey s) (R.EvaluationKey e) -> join ["CommentFromModifyEvaluation:", s, ":", e])
          (\(R.CourseKey c) -> join $ ["DeleteUsersFromCourse:", c])
          (\(R.GroupKey g) -> join $ ["DeleteUsersFromGroup:", g])
          (\(R.GroupKey g) -> join $ ["UnsubscribeFromCourse:", g])

instance SessionRestore P.Page where
  restoreFromSession kv = case L.lookup pageSessionKey kv of
    Nothing           -> Nothing
    Just "Login"      -> Just P.Login
    Just "Logout"     -> Just P.Logout
    Just "Home"       -> Just P.Home
    Just "Error"      -> Just P.Error
    Just "Profile"    -> Just P.Profile
    Just "CourseAdmin" -> Just P.CourseAdmin
    Just "EvaluationTable" -> Just P.EvaluationTable
    Just "Submission"      -> Just P.Submission
    Just "SubmissionList"  -> Just P.SubmissionList
    Just "UserSubmissions" -> Just P.UserSubmissions
    Just "Administration"   -> Just P.Administration
    Just "GroupRegistration" -> Just P.GroupRegistration
    Just "CreateCourse" -> Just P.CreateCourse
    Just "UserDetails"  -> Just P.UserDetails
    Just "AssignCourseAdmin" -> Just P.AssignCourseAdmin
    Just "CreateGroup"  -> Just P.CreateGroup
    Just "AssignGroupAdmin" -> Just P.AssignGroupAdmin
    Just "NewCourseAssignment" -> Just P.NewCourseAssignment
    Just "NewGroupAssignment" -> Just P.NewGroupAssignment
    Just "ModifyAssignment" -> Just P.ModifyAssignment
    Just "ChangePassword" -> Just P.ChangePassword
    Just "SetUserPassword" -> Just P.SetUserPassword
    Just ts
      | startsWith "CommentFromEvaluation:" ts ->
          Just . P.CommentFromEvaluation . submissionKey $ dropPreffix "CommentFromEvaluation:" ts

      | startsWith "CommentFromModifyEvaluation:" ts ->
          let se = splitValues "CommentFromModifyEvaluation:" ts
          in case se of
              [s,e] -> Just $ P.CommentFromModifyEvaluation (submissionKey s) (evaluationKey e)
              _     -> Nothing

      | startsWith "SubmissionDetails:" ts ->
          let as = splitValues "SubmissionDetails:" ts
          in case as of
               [a,s] -> Just $ P.SubmissionDetails (assignmentKey a) (submissionKey s)
               _     -> Nothing

      | startsWith "Evaluation:" ts ->
          Just . P.Evaluation . submissionKey $ dropPreffix "Evaluation:" ts

      | startsWith "ModifyEvaluation:" ts ->
          let se = splitValues "ModifyEvaluation:" ts
          in case se of
              [s,e] -> Just $ P.ModifyEvaluation (submissionKey s) (evaluationKey e)
              _     -> Nothing

      | startsWith "DeleteUsersFromCourse:" ts ->
          let se = splitValues "DeleteUsersFromCourse:" ts
          in case se of
               [c] -> Just $ P.DeleteUsersFromCourse (courseKey c)
               _   -> Nothing

      | startsWith "DeleteUsersFromGroup:" ts ->
          let se = splitValues "DeleteUsersFromGroup:" ts
          in case se of
               [g] -> Just $ P.DeleteUsersFromGroup (groupKey g)
               _   -> Nothing

      | startsWith "UnsubscribeFromCourse:" ts ->
          let se = splitValues "UnsubscribeFromCourse:" ts
          in case se of
               [g] -> Just $ P.UnsubscribeFromCourse (groupKey g)
               _   -> Nothing

    Just _ -> Nothing
    where
      assignmentKey = R.AssignmentKey . T.unpack

      submissionKey = R.SubmissionKey . T.unpack

      evaluationKey = R.EvaluationKey . T.unpack

      courseKey = R.CourseKey . T.unpack

      groupKey = R.GroupKey . T.unpack

      username = E.Username . T.unpack

      startsWith preffix t = preffix == (T.take (T.length preffix) t)

      dropPreffix preffix t = T.drop (T.length preffix) t

      splitValues preffix t = T.splitOn ":" $ dropPreffix preffix t

-- * Session Key Values for Username

usernameSessionKey :: T.Text
usernameSessionKey = "Username"

instance SessionStore E.Username where
  sessionStore (E.Username n) = [(usernameSessionKey, T.pack n)]

instance SessionRestore E.Username where
  restoreFromSession kv = case L.lookup usernameSessionKey kv of
    Nothing -> Nothing
    Just v -> Just $ E.Username $ T.unpack v

-- * Session Key Values for Language

languageSessionKey :: T.Text
languageSessionKey = "Language"

instance SessionStore Language where
  sessionStore (Language l) = [(languageSessionKey, T.pack l)]

instance SessionRestore Language where
  restoreFromSession kv = (Language . T.unpack) <$> (L.lookup languageSessionKey kv)

-- * Session handlers

sessionVersionKey :: T.Text
sessionVersionKey = "Version"

sessionVersionValue :: T.Text
sessionVersionValue = "1"

newtype SessionVersion = SessionVersion T.Text
  deriving (Eq)

sessionVersion = SessionVersion sessionVersionValue

instance SessionRestore SessionVersion where
  restoreFromSession kv = case L.lookup sessionVersionKey kv of
    Nothing -> Nothing
    Just v -> Just . SessionVersion $ v

setInSessionKeyValues :: [(T.Text, T.Text)] -> Handler App SessionManager ()
setInSessionKeyValues = mapM_ (\(key,value) -> setInSession key value)

fromSession :: (SessionRestore r) => T.Text -> Handler App SessionManager (Maybe r)
fromSession key = do
  v <- getFromSession key
  return $ join $ fmap (restoreFromSession . (\v' -> [(key,v')])) v

getSessionVersion :: Handler App SessionManager (Maybe SessionVersion)
getSessionVersion = fromSession sessionVersionKey

setSessionVersion :: Handler App SessionManager ()
setSessionVersion = setInSessionKeyValues [(sessionVersionKey, sessionVersionValue)]

usernameFromSession :: Handler App SessionManager (Maybe E.Username)
usernameFromSession = fromSession usernameSessionKey

setUsernameInSession :: Username -> Handler App SessionManager ()
setUsernameInSession = setInSessionKeyValues . sessionStore

actPageFromSession :: Handler App SessionManager (Maybe P.Page)
actPageFromSession = fromSession pageSessionKey

setActPageInSession :: P.Page -> Handler App SessionManager ()
setActPageInSession = setInSessionKeyValues . sessionStore

languageFromSession :: Handler App SessionManager (Maybe Language)
languageFromSession = fromSession languageSessionKey

setLanguageInSession :: Language -> Handler App SessionManager ()
setLanguageInSession = setInSessionKeyValues . sessionStore

-- * Username and UserState correspondence

usernameFromAuthUser :: AuthUser -> Username
usernameFromAuthUser = E.Username . (T.unpack) . A.userLogin

passwordFromAuthUser :: AuthUser -> Maybe E.Password
passwordFromAuthUser = fmap asPassword . userPassword

authPasswordCata :: (ByteString -> a) -> (ByteString -> a) -> A.Password -> a
authPasswordCata clear _   (A.ClearText t) = clear   t
authPasswordCata _ encrypt (A.Encrypted e) = encrypt e

convertPassword :: A.Password -> String
convertPassword = authPasswordCata unpack unpack

instance AsUsername ByteString where
  asUsername = E.Username . unpack

instance AsPassword ByteString where
  asPassword = unpack

instance AsPassword A.Password where
  asPassword (A.ClearText t) = unpack t
  asPassword (A.Encrypted e) = unpack e

-- * Debugging

sessionCookies :: Handler App SessionManager String
sessionCookies = do
  as <- sessionToList
  return . join . join . L.map (\(k,v) -> ["(KEY: ",T.unpack k,",","VALUE: ",T.unpack v,")"]) $ as

#ifdef TEST

-- * Invariants

invariants :: Invariants P.Page
invariants = Invariants [
    ("Each page must have session store value defined", \p -> 
        case (sessionStore p) of
          [] -> False
          [(key, val)] -> and [T.length key > 0, T.length val > 0])
  , ("Each page description must be restorable from session cookie", \p ->
        case (restoreFromSession (sessionStore p)) of
          Nothing -> False
          Just p' -> p == p')
  ]

sessionKeys = [
    pageSessionKey
  , usernameSessionKey
  , languageSessionKey
  , sessionVersionKey
  ]

unitTests :: UnitTests
unitTests = UnitTests [
    ("Each session key must be different", (length (L.nub sessionKeys) == (length sessionKeys)))
  ]

#endif
