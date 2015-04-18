{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
module Bead.View.Session where

import           Data.ByteString.Char8 hiding (index, length)
import qualified Data.Text as T
import qualified Data.List as L
import           Control.Applicative
import           Control.Monad

import           Bead.Domain.Entities as E
import           Bead.View.BeadContext

import           Snap hiding (get)
import           Snap.Snaplet.Auth as A
import           Snap.Snaplet.Session

#ifdef TEST
import           Bead.Invariants (UnitTests(..))
#endif

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
removeSessionKeys :: (T.Text -> Bool) -> BeadHandler' b ()
removeSessionKeys pred = withTop sessionManager $ do
  values <- L.filter (not . pred . fst) <$> sessionToList
  resetSession
  setInSessionKeyValues values
  commitSession

-- Clears the private session data from the session
resetPrivateSessionData :: Handler BeadContext b ()
resetPrivateSessionData = withTop sessionManager $ removeSessionKeys isPrivateKey

-- Clears the public session data from the session
resetPublicSessionData :: Handler BeadContext b ()
resetPublicSessionData = withTop sessionManager $ removeSessionKeys isPublicKey

-- * Session Key and Values for Page

pageSessionKey :: T.Text
pageSessionKey = "Page"

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

setInSessionKeyValues :: [(T.Text, T.Text)] -> BeadHandler' b ()
setInSessionKeyValues = withTop sessionManager . mapM_ (\(key,value) -> setInSession key value)

fromSession :: (SessionRestore r) => T.Text -> BeadHandler' b (Maybe r)
fromSession key = withTop sessionManager $ do
  v <- getFromSession key
  return $ join $ fmap (restoreFromSession . (\v' -> [(key,v')])) v

getSessionVersion :: BeadHandler' b (Maybe SessionVersion)
getSessionVersion = fromSession sessionVersionKey

setSessionVersion :: BeadHandler' b ()
setSessionVersion = setInSessionKeyValues [(sessionVersionKey, sessionVersionValue)]

usernameFromSession :: BeadHandler' b (Maybe E.Username)
usernameFromSession = fromSession usernameSessionKey

setUsernameInSession :: Username -> BeadHandler' b ()
setUsernameInSession = setInSessionKeyValues . sessionStore

languageFromSession :: BeadHandler' b (Maybe Language)
languageFromSession = fromSession languageSessionKey

setLanguageInSession :: Language -> BeadHandler' b ()
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

sessionCookies :: BeadHandler' b String
sessionCookies = withTop sessionManager $ do
  as <- sessionToList
  return . join . join . L.map (\(k,v) -> ["(KEY: ",T.unpack k,",","VALUE: ",T.unpack v,")"]) $ as

-- * Helper functions to be able to modify session on the top level

commitSessionTop :: BeadHandler' b ()
commitSessionTop = withTop sessionManager commitSession

resetSessionTop :: BeadHandler' b ()
resetSessionTop = withTop sessionManager resetSession

touchSessionTop :: BeadHandler' b ()
touchSessionTop = withTop sessionManager touchSession

csrfTokenTop :: BeadHandler' b T.Text
csrfTokenTop = withTop sessionManager csrfToken

setInSessionTop :: T.Text -> T.Text -> BeadHandler' b ()
setInSessionTop k v = withTop sessionManager $ setInSession k v

#ifdef TEST

-- * Invariants

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
