{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Snap.Session where

-- Bead imports

import Bead.Domain.Entities as E
import qualified Bead.Controller.Pages as P
import Bead.View.Snap.Application (App)

import Bead.Invariants (Invariants(..))

-- Haskell imports

import Control.Monad (join)
import Data.ByteString.Char8 hiding (index)
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
    s P.Profile    = "Profile"
    s P.Course     = "Course"
    s P.Courses    = "Courses"
    s P.Group      = "Group"
    s P.Groups     = "Groups"
    s P.Exercise   = "Exercise"
    s P.ClosedExam = "ClosedExam"
    s P.Error      = "Error"
    s P.SubmitExam = "SubmitExam"
    s P.Evaulation = "Evaulation"
    s P.Training   = "Training"
    s P.Admin      = "Admin"
    s P.Users      = "Users"
    s P.UserDetails    = "UserDetails"
    s P.CreateExercise = "CreateExercise"
    s P.CreateCourse   = "CreateCourse"
    s P.CreateGroup    = "CreateGroup"

instance SessionRestore P.Page where
  restoreFromSession kv = case L.lookup pageSessionKey kv of
    Nothing           -> Nothing
    Just "Login"      -> Just P.Login
    Just "Logout"     -> Just P.Logout
    Just "Home"       -> Just P.Home
    Just "Profile"    -> Just P.Profile
    Just "Course"     -> Just P.Course
    Just "Courses"    -> Just P.Courses
    Just "Group"      -> Just P.Group
    Just "Groups"     -> Just P.Groups
    Just "Exercise"   -> Just P.Exercise
    Just "ClosedExam" -> Just P.ClosedExam
    Just "Error"      -> Just P.Error
    Just "SubmitExam" -> Just P.SubmitExam
    Just "Evaulation" -> Just P.Evaulation
    Just "Training"   -> Just P.Training
    Just "Admin"      -> Just P.Admin
    Just "Users"      -> Just P.Users
    Just "UserDetails" -> Just P.UserDetails
    Just "CreateExercise" -> Just P.CreateExercise
    Just "CreateCourse"   -> Just P.CreateCourse
    Just "CreateGroup"    -> Just P.CreateGroup

-- * Session Key Values for Username

usernameSessionKey :: T.Text
usernameSessionKey = "Username"

instance SessionStore E.Username where
  sessionStore (E.Username n) = [(usernameSessionKey, T.pack n)]

instance SessionRestore E.Username where
  restoreFromSession kv = case L.lookup usernameSessionKey kv of
    Nothing -> Nothing
    Just v -> Just $ E.Username $ T.unpack v

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

-- * Username and UserState correspondence

usernameFromAuthUser :: AuthUser -> Username
usernameFromAuthUser = E.Username . (T.unpack) . A.userLogin

passwordFromAuthUser :: AuthUser -> E.Password
passwordFromAuthUser a = case userPassword a of
  Just p  -> asPassword p
  Nothing -> error "passwordFromAuthUser: No password was given"

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
