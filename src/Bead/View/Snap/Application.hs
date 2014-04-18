{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Application where

import           Control.Lens.TH
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.IORef
import qualified Data.Map as Map
import           Data.String (fromString)
import qualified Data.Text as DT
import qualified Data.Text.Lazy as LT
import           Network.Mail.Mime
import           System.Random
import           Text.Regex.TDFA

import           Snap hiding (Config(..))
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Fay
import           Snap.Snaplet.Session

import           Bead.Configuration (Config(..))
import           Bead.Controller.ServiceContext
import           Bead.Daemon.Email as EmailDaemon
import           Bead.Daemon.Logout
import           Bead.Domain.Entities
import           Bead.View.Snap.Dictionary
import           Bead.View.Snap.EmailTemplate

-- * Mini snaplet : Service context

newtype SnapContext c = SnapContext (IORef c)

makeSnapContext :: String -> String -> c -> SnapletInit b (SnapContext c)
makeSnapContext name desc c = makeSnaplet
  (fromString name)
  (fromString desc)
  Nothing $
  liftIO $ do
    ref <- newIORef c
    return $! SnapContext ref

snapContextCata :: (c -> a) -> Handler b (SnapContext c) a
snapContextCata f = do
  SnapContext ref <- get
  f <$> (liftIO $ readIORef ref)

snapContextHandlerCata :: (c -> Handler b (SnapContext c) a) -> Handler b (SnapContext c) a
snapContextHandlerCata k = do
  SnapContext ref <- get
  (liftIO $ readIORef ref) >>= k

type ConfigServiceContext = SnapContext Config

configurationServiceContext :: Config -> SnapletInit b ConfigServiceContext
configurationServiceContext = makeSnapContext
  "Configuration"
  "A snaplet providin the service context of the configuration"

getConfiguration :: Handler b ConfigServiceContext Config
getConfiguration = snapContextCata id

type SnapletServiceContext = SnapContext (ServiceContext, LogoutDaemon)

contextSnaplet :: ServiceContext -> LogoutDaemon -> SnapletInit b SnapletServiceContext
contextSnaplet s l = makeSnapContext
  "ServiceContext"
  "A snaplet providing the service context of the user stories"
  (s,l)

getServiceContext :: Handler b SnapletServiceContext ServiceContext
getServiceContext = snapContextCata fst

getLogoutDaemon :: Handler b SnapletServiceContext LogoutDaemon
getLogoutDaemon = snapContextCata snd

getServiceContextAndLogoutDaemon :: Handler b SnapletServiceContext (ServiceContext, LogoutDaemon)
getServiceContextAndLogoutDaemon = snapContextCata id

-- * Mini snaplet : Dictionary snaplet

type DictionaryContext = SnapContext (Dictionaries, Language)

-- Create a Dictionary context from the given dictionaries and a defualt language
dictionarySnaplet :: Dictionaries -> Language -> SnapletInit a DictionaryContext
dictionarySnaplet d l = makeSnapContext
  "Dictionaries"
  "A snaplet providing the i18 dictionary context"
  ((addDefault d), l)
  where
    -- The source code contains english comments by default
    addDefault = Map.insert (Language "en") (idDictionary, DictionaryInfo "en.ico" "English")

-- Calculates the default language which comes from the configuration
configuredDefaultDictionaryLanguage :: Handler b DictionaryContext Language
configuredDefaultDictionaryLanguage = snapContextCata snd

-- | getDictionary returns a (Just dictionary) for the given language
--   if the dictionary is registered for the given language,
--   otherwise returns Nothing
getDictionary :: Language -> Handler b DictionaryContext (Maybe Dictionary)
getDictionary l = snapContextCata (\(d,_l) -> fmap fst $ Map.lookup l d)

-- A dictionary infos is a list that contains the language of and information
-- about the dictionaries contained by the DictionarySnaplet
type DictionaryInfos = [(Language, DictionaryInfo)]

dictionaryInfosCata list item d = list $ map item d

-- Computes a list with the defined languages and dictionary info
dcGetDictionaryInfos :: Handler b DictionaryContext DictionaryInfos
dcGetDictionaryInfos = snapContextCata (\(d,l) -> Map.toList $ Map.map snd d)

-- * Email sending snaplet

type Subject = String -- The subject of an email message
type Message = String -- The content of an email message

-- Email sender function get a string and en email address
-- and sends the email to the address
type EmailSender = Email -> Subject -> Message -> IO ()

-- SendEmailContext is a reference to the email sender function, we keep only
-- one of the email senders.
type SendEmailContext = SnapContext EmailSender

verySimpleMail :: Address -> Address -> DT.Text -> LT.Text -> IO Mail
verySimpleMail to from subject plainBody = return Mail
        { mailFrom = from
        , mailTo   = [to]
        , mailCc   = []
        , mailBcc  = []
        , mailHeaders = [ ("Subject", subject) ]
        , mailParts =
            [[ Part "text/plain; charset=UTF-8" Base64 Nothing []
             $ BL.pack . LT.unpack $ plainBody
            ]]
        }

emailSenderSnaplet :: Config -> EmailDaemon -> SnapletInit a SendEmailContext
emailSenderSnaplet config daemon = makeSnapContext
  "Email sending"
  "A snaplet providing email sender functionality"
  sender
  where
    sender :: Email -> Subject -> Message -> IO ()
    sender address sub msg = do
      let from = Address Nothing (fromString $ emailFromAddress config)
          to   = Address Nothing (emailFold fromString address)
          subject = fromString sub
          plain   = fromString msg
      mail <- verySimpleMail to from subject plain
      EmailDaemon.sendEmail daemon mail

-- Send email with a subject to the given address, using the right
-- template to the given values
-- E.g: Registration or ForgottenPassword
sendEmail :: (Template t)
  => Email -> Subject -> Message -> t -> Handler b SendEmailContext ()
sendEmail address sub body value = snapContextHandlerCata $ \send -> do
  msg <- liftIO . runEmailTemplate (emailTemplate body) $ value
  liftIO $ send address sub msg

-- * Bead's temp directory

-- Bead temp directory holds a reference to the created temp directory
-- where the handlers can place temporary files
type TempDirectoryContext = SnapContext FilePath

tempDirectorySnaplet :: FilePath -> SnapletInit a TempDirectoryContext
tempDirectorySnaplet = makeSnapContext
  "Template directory"
  "A snaplet holding a reference to the temporary directory"

-- Returns the bead temp directory
getTempDirectory :: Handler b TempDirectoryContext FilePath
getTempDirectory = snapContextCata id

-- * Username check

-- Checks the given username against some IO computation based checker
type CheckUsernameContext = SnapContext (String -> IO Bool)

regexpUsernameChecker :: Config -> SnapletInit a CheckUsernameContext
regexpUsernameChecker cfg = makeSnaplet
  "Regexp username checker"
  "A snaplet providing checks against the regular expression defined in the configuration file"
  Nothing $ liftIO $ do
    let pattern = usernameRegExp cfg
    ref <- newIORef (\username -> return $ check username pattern)
    return $! SnapContext ref
  where
    check :: String -> String -> Bool
    check usr ptn = usr =~ ptn

-- Returns True, if the username pass the check otherwise False
checkUsername :: String -> Handler b CheckUsernameContext Bool
checkUsername usr = snapContextHandlerCata $ \f -> liftIO (f usr)

-- * Password generation

-- PasswordGeneratorContext is a reference to the password generator computation,
-- we keep only one of it
type PasswordGeneratorContext = SnapContext (IO String)

passwordGeneratorSnaplet :: SnapletInit a PasswordGeneratorContext
passwordGeneratorSnaplet = makeSnaplet
  "Password generation"
  "A snaplet providing password generation functionality"
  Nothing $ liftIO $ do
    pwdGen <- createPasswordGenerator
    ref <- newIORef pwdGen
    return $! SnapContext ref

-- Generates a new password string
getRandomPassword :: Handler b PasswordGeneratorContext String
getRandomPassword = snapContextHandlerCata liftIO

-- Creates a password generator that generates 12 length passwords containing
-- upper, lowercase letters, and digits.
createPasswordGenerator :: IO (IO String)
createPasswordGenerator = do
  std <- newStdGen
  stdRef <- newIORef std
  let nextValue = do
        s <- readIORef stdRef
        let (x,s') = random s
        writeIORef stdRef s'
        return x

  let passwordGenerator = replicateM 12 $ do
        type_ <- fmap (`mod` 3) nextValue
        case type_ of
          0 -> fmap lowerCase nextValue
          1 -> fmap upperCase nextValue
          2 -> fmap digit     nextValue

  return passwordGenerator

  where
    lowerCase :: Int -> Char
    lowerCase n = ['a'..'z'] !! (mod n 26)

    upperCase :: Int -> Char
    upperCase n = ['A'..'Z'] !! (mod n 26)

    digit :: Int -> Char
    digit n = ['0'..'9'] !! (mod n 10)

-- * Application

data App = App {
    _sessionManager :: Snaplet SessionManager
  , _auth           :: Snaplet (AuthManager App)
  , _serviceContext :: Snaplet SnapletServiceContext
  , _dictionaryContext :: Snaplet DictionaryContext
  , _sendEmailContext   :: Snaplet SendEmailContext
  , _randomPasswordContext :: Snaplet PasswordGeneratorContext
  , _fayContext     :: Snaplet Fay
  , _tempDirContext :: Snaplet TempDirectoryContext
  , _configContext  :: Snaplet ConfigServiceContext
  , _checkUsernameContext :: Snaplet CheckUsernameContext
  }

makeLenses ''App

