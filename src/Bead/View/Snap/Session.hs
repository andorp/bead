{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
module Bead.View.Snap.Session where

-- Bead imports

import Bead.Domain.Entities as E
import qualified Bead.Controller.Pages as P
import Bead.View.Snap.Application (App)
import Bead.View.Snap.Dictionary (Language(..))
#ifdef TEST
import Bead.Invariants (Invariants(..), UnitTests(..))
#endif

-- Haskell imports

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

-- * Session Key and Values for Page

pageSessionKey :: T.Text
pageSessionKey = "Page"

instance SessionStore P.Page where
  sessionStore p = [(pageSessionKey, T.pack $ s p)] where
    s P.Login      = "Login"
    s P.Logout     = "Logout"
    s P.Home       = "Home"
    s P.Error      = "Error"
    s P.Profile    = "Profile"
    s P.CourseAdmin = "CourseAdmin"
    s P.EvaulationTable = "EvaulationTable"
    s P.Evaulation      = "Evaulation"
    s P.Submission      = "Submission"
    s P.SubmissionList  = "SubmissionList"
    s P.UserSubmissions = "UserSubmissions"
    s P.SubmissionDetails = "SubmissionDetails"
    s P.ModifyEvaulation  = "ModifyEvaulation"
    s P.Administration   = "Administration"
    s P.GroupRegistration = "GroupRegistration"
    s P.CreateCourse = "CreateCourse"
    s P.UserDetails = "UserDetails"
    s P.AssignCourseAdmin = "AssignCourseAdmin"
    s P.CreateGroup = "CreateGroup"
    s P.AssignProfessor = "AssignProfessor"
    s P.NewGroupAssignment  = "NewGroupAssignment"
    s P.NewCourseAssignment = "NewCourseAssignment"
    s P.ModifyAssignment = "ModifyAssignment"

instance SessionRestore P.Page where
  restoreFromSession kv = case L.lookup pageSessionKey kv of
    Nothing           -> Nothing
    Just "Login"      -> Just P.Login
    Just "Logout"     -> Just P.Logout
    Just "Home"       -> Just P.Home
    Just "Error"      -> Just P.Error
    Just "Profile"    -> Just P.Profile
    Just "CourseAdmin" -> Just P.CourseAdmin
    Just "EvaulationTable" -> Just P.EvaulationTable
    Just "Evaulation"      -> Just P.Evaulation
    Just "Submission"      -> Just P.Submission
    Just "SubmissionList"  -> Just P.SubmissionList
    Just "UserSubmissions" -> Just P.UserSubmissions
    Just "SubmissionDetails" -> Just P.SubmissionDetails
    Just "ModifyEvaulation"  -> Just P.ModifyEvaulation
    Just "Administration"   -> Just P.Administration
    Just "GroupRegistration" -> Just P.GroupRegistration
    Just "CreateCourse" -> Just P.CreateCourse
    Just "UserDetails"  -> Just P.UserDetails
    Just "AssignCourseAdmin" -> Just P.AssignCourseAdmin
    Just "CreateGroup"  -> Just P.CreateGroup
    Just "AssignProfessor" -> Just P.AssignProfessor
    Just "NewCourseAssignment" -> Just P.NewCourseAssignment
    Just "NewGroupAssignment" -> Just P.NewGroupAssignment
    Just "ModifyAssignment" -> Just P.ModifyAssignment
    Just _              -> Nothing

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
