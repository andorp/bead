{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Bead.View.Snap.Application where

import Snap hiding (Config(..))
import Snap.Snaplet.Session
import Snap.Snaplet.Auth
import Snap.Snaplet.Fay
import Control.Lens.TH
import Data.IORef
import qualified Data.Map as Map

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String (fromString)
import qualified Data.Text as DT
import qualified Data.Text.Lazy as LT
import Network.Mail.Mime
import System.Random

import Bead.Configuration (Config(..))
import Bead.Domain.Entities
import Bead.View.Snap.Dictionary
import Bead.View.Snap.EmailTemplate
import Bead.Controller.LogoutDaemon
import Bead.Controller.ServiceContext

-- * Mini snaplet : Service context

newtype SrvContext c = SrvContext (IORef c)

createServiceContext :: String -> String -> c -> SnapletInit b (SrvContext c)
createServiceContext name desc c = makeSnaplet
  (fromString name)
  (fromString desc)
  Nothing $
  liftIO $ do
    ref <- newIORef c
    return $! SrvContext ref

serviceContextCata :: (c -> a) -> Handler b (SrvContext c) a
serviceContextCata f = do
  SrvContext ref <- get
  f <$> (liftIO $ readIORef ref)

type ConfigServiceContext = SrvContext Config

configurationServiceContext :: Config -> SnapletInit b ConfigServiceContext
configurationServiceContext = createServiceContext
  "Configuration"
  "A snaplet providin the service context of the configuration"

getConfiguration :: Handler b ConfigServiceContext Config
getConfiguration = serviceContextCata id

newtype SnapletServiceContext = SnapletServiceContext (IORef (ServiceContext, LogoutDaemon))

contextSnaplet :: ServiceContext -> LogoutDaemon -> SnapletInit b SnapletServiceContext
contextSnaplet s l = makeSnaplet
  "ServiceContext"
  "A snaplet providing the service context of the user stories"
  Nothing $
  liftIO $ do
    ref <- newIORef (s,l)
    return $! SnapletServiceContext ref

getServiceContext :: Handler b SnapletServiceContext ServiceContext
getServiceContext = do
  SnapletServiceContext ref <- get
  fmap fst $ liftIO . readIORef $ ref

getLogoutDaemon :: Handler b SnapletServiceContext LogoutDaemon
getLogoutDaemon = do
  SnapletServiceContext ref <- get
  fmap snd $ liftIO . readIORef $ ref

getServiceContextAndLogoutDaemon :: Handler b SnapletServiceContext (ServiceContext, LogoutDaemon)
getServiceContextAndLogoutDaemon = do
  SnapletServiceContext ref <- get
  liftIO . readIORef $ ref

-- * Mini snaplet : Dictionary snaplet

newtype DictionaryContext = DictionaryContext (IORef (Dictionaries, Language))

-- Create a Dictionary context from the given dictionaries and a defualt language
dictionarySnaplet :: Dictionaries -> Language -> SnapletInit a DictionaryContext
dictionarySnaplet d l = makeSnaplet
  "Dictionaries"
  "A snaplet providing the i18 dictionary context"
  Nothing $ liftIO $ do
    ref <- newIORef ((addDefault d), l)
    return $! DictionaryContext ref
  where
    -- The source code contains english comments by default
    addDefault = Map.insert (Language "en") (idDictionary, DictionaryInfo "en.ico" "English")

-- Maps the stored dictionaries into a value within the Handler monad
dictionarySnapletMap :: (Dictionaries -> Language -> a) -> Handler b DictionaryContext a
dictionarySnapletMap f = do
  DictionaryContext ref <- get
  (m, l) <- liftIO . readIORef $ ref
  return (f m l)

-- Calculates the default language which comes from the configuration
configuredDefaultDictionaryLanguage :: Handler b DictionaryContext Language
configuredDefaultDictionaryLanguage = dictionarySnapletMap (\_dic lang -> lang)

-- | getDictionary returns a (Just dictionary) for the given language
--   if the dictionary is registered for the given language,
--   otherwise returns Nothing
getDictionary :: Language -> Handler b DictionaryContext (Maybe Dictionary)
getDictionary l = dictionarySnapletMap (\d _l -> fmap fst $ Map.lookup l d)

-- A dictionary infos is a list that contains the language of and information
-- about the dictionaries contained by the DictionarySnaplet
type DictionaryInfos = [(Language, DictionaryInfo)]

dictionaryInfosCata list item d = list $ map item d

-- Computes a list with the defined languages and dictionary info
dcGetDictionaryInfos :: Handler b DictionaryContext DictionaryInfos
dcGetDictionaryInfos = dictionarySnapletMap (\d l -> Map.toList $ Map.map snd d)

-- * Email sending snaplet

type Subject = String -- The subject of an email message
type Message = String -- The content of an email message

-- Email sender function get a string and en email address
-- and sends the email to the address
type EmailSender = Email -> Subject -> Message -> IO ()

-- SendEmailContext is a reference to the email sender function, we keep only
-- one of the email senders.
newtype SendEmailContext = SendEmailContext (IORef EmailSender)

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

emailSenderSnaplet :: Config -> SnapletInit a SendEmailContext
emailSenderSnaplet config = makeSnaplet
  "Email sending"
  "A snaplet providing email sender functionality"
  Nothing $ liftIO $ do
    ref <- newIORef sender
    return $! SendEmailContext ref
  where
    sender :: Email -> Subject -> Message -> IO ()
    sender address sub msg = do
      let from = Address Nothing (fromString $ emailFromAddress config)
          to   = Address Nothing (emailFold fromString address)
          subject = fromString sub
          plain   = fromString msg
      mail <- verySimpleMail to from subject plain
      renderSendMail mail

-- Send email with a subject to the given address, using the right
-- template to the given values
-- E.g: Registration or ForgottenPassword
sendEmail :: (Template t)
  => Email -> Subject -> Message -> t -> Handler b SendEmailContext ()
sendEmail address sub body value = do
  SendEmailContext ref <- get
  send <- liftIO . readIORef $ ref
  msg <- liftIO . runEmailTemplate (emailTemplate body) $ value
  liftIO $ send address sub msg

-- * Bead's temp directory

-- Bead temp directory holds a reference to the created temp directory
-- where the handlers can place temporary files
newtype TempDirectoryContext = TempDirectoryContext (IORef FilePath)

tempDirectorySnaplet :: FilePath -> SnapletInit a TempDirectoryContext
tempDirectorySnaplet tempPath = makeSnaplet
  "Template directory"
  "A snaplet holding a reference to the temporary directory"
  Nothing $ liftIO $ do
    ref <- newIORef tempPath
    return $! TempDirectoryContext ref

-- Returns the bead temp directory
getTempDirectory :: Handler b TempDirectoryContext FilePath
getTempDirectory = do
  TempDirectoryContext ref <- get
  liftIO $ readIORef ref

-- * Password generation

-- PasswordGeneratorContext is a reference to the password generator computation,
-- we keep only one of it
newtype PasswordGeneratorContext = PasswordGeneratorContext (IORef (IO String))

passwordGeneratorSnaplet :: SnapletInit a PasswordGeneratorContext
passwordGeneratorSnaplet = makeSnaplet
  "Password generation"
  "A snaplet providing password generation functionality"
  Nothing $ liftIO $ do
    pwdGen <- createPasswordGenerator
    ref <- newIORef pwdGen
    return $! PasswordGeneratorContext ref

-- Generates a new password string
getRandomPassword :: Handler b PasswordGeneratorContext String
getRandomPassword = do
  PasswordGeneratorContext ref <- get
  gen <- liftIO . readIORef $ ref
  liftIO gen

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
  }

makeLenses ''App

