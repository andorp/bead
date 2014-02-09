{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Bead.View.Snap.Application where

import Snap hiding (Config(..))
import Snap.Snaplet
import Snap.Snaplet.Session
import Snap.Snaplet.Auth
import Snap.Snaplet.Fay
import Control.Lens.TH
import Data.IORef
import qualified Data.Map as Map

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String (fromString)
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DT
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Text.XmlHtml as X
import Network.Mail.Mime
import System.Random

import Bead.Configuration (Config(..))
import Bead.Domain.Entities
import Bead.View.Snap.Dictionary
import Bead.View.Snap.TemplateAndComponentNames
import Bead.View.Snap.EmailTemplate
import Bead.Controller.ServiceContext

-- * Mini snaplet : Service context

newtype SnapletServiceContext = SnapletServiceContext (IORef ServiceContext)

contextSnaplet :: ServiceContext -> SnapletInit b SnapletServiceContext
contextSnaplet s = makeSnaplet
  "ServiceContext"
  "A snaplet providing the service context of the user stories"
  Nothing $
  liftIO $ do
    ref <- newIORef s
    return $! SnapletServiceContext ref

getServiceContext :: Handler b SnapletServiceContext ServiceContext
getServiceContext = do
  SnapletServiceContext ref <- get
  liftIO . readIORef $ ref

-- * Mini snaplet : Dictionary snaplet

newtype DictionaryContext = DictionaryContext (IORef Dictionaries)

dictionarySnaplet :: Dictionaries -> SnapletInit a DictionaryContext
dictionarySnaplet d = makeSnaplet
  "Dictionaries"
  "A snaplet providing the i18 dictionary context"
  Nothing $ liftIO $ do
    ref <- newIORef (addHungarian d)
    return $! DictionaryContext ref
  where
    -- The source code contains hungarian text by default
    addHungarian = Map.insert (Language "hu") (idDictionary, DictionaryInfo "hu.ico" "Magyar")

-- Maps the stored dictionaries into a value within the Handler monad
dictionarySnapletMap :: (Dictionaries -> a) -> Handler b DictionaryContext a
dictionarySnapletMap f = do
  DictionaryContext ref <- get
  m <- liftIO . readIORef $ ref
  return (f m)

-- | getDictionary returns a (Just dictionary) for the given language
--   if the dictionary is registered for the given language,
--   otherwise returns Nothing
getDictionary :: Language -> Handler b DictionaryContext (Maybe Dictionary)
getDictionary l = dictionarySnapletMap (fmap fst . Map.lookup l)

-- A dictionary infos is a list that contains the language of and information
-- about the dictionaries contained by the DictionarySnaplet
type DictionaryInfos = [(Language, DictionaryInfo)]

dictionaryInfosCata list item d = list $ map item d

-- Computes a list with the defined languages and dictionary info
dcGetDictionaryInfos :: Handler b DictionaryContext DictionaryInfos
dcGetDictionaryInfos = dictionarySnapletMap (Map.toList . Map.map snd)

-- * Email sending spanplet

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
  }

makeLenses ''App

