{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Bead.View.Snap.Application where

import Snap
import Snap.Snaplet
import Snap.Snaplet.Session
import Snap.Snaplet.Auth
import Snap.Snaplet.Fay
import Control.Lens.TH
import Data.IORef
import qualified Data.Map as Map

import Data.String (fromString)
import qualified Data.Text as DT
import qualified Text.XmlHtml as X
import Network.Mail.Mime

import Bead.Domain.Entities
import Bead.View.Snap.Dictionary
import Bead.View.Snap.TemplateAndComponentNames
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
    ref <- newIORef d
    return $! DictionaryContext ref

-- | getDictionary returns a (Just dictionary) for the given language
--   if the dictionary is registered for the given language,
--   otherwise returns Nothing
getDictionary :: Language -> Handler b DictionaryContext (Maybe Dictionary)
getDictionary l = do
  DictionaryContext ref <- get
  m <- liftIO . readIORef $ ref
  return $ Map.lookup l m

-- * Email sending spanplet

type Subject = String
type Message = String

-- Email sender function get a string and en email address
-- and sends the email to the address
type EmailSender = Email -> Subject -> Message -> IO ()

-- SendEmailContext is a reference to the email sender function, we keep only
-- one of the email senders.
newtype SendEmailContext = SendEmailContext (IORef EmailSender)

emailSenderSnaplet :: SnapletInit a SendEmailContext
emailSenderSnaplet = makeSnaplet
  "Email sending"
  "A snaplet providing email sender functionality"
  Nothing $ liftIO $ do
    ref <- newIORef sender
    return $! SendEmailContext ref
  where
    sender :: Email -> String -> String -> IO ()
    sender address sub msg = do
      let from = Address (Just "noreply") "noreply@bead.com"
          to   = Address Nothing (emailFold fromString address)
          subject = fromString sub
          plain   = fromString msg
          html    = fromString ""
      mail <- simpleMail to from subject plain html []
      renderSendMail mail

-- Send email with subject to the given address
sendEmail :: Email -> Subject -> Message -> Handler b SendEmailContext ()
sendEmail address sub msg = do
  SendEmailContext ref <- get
  send <- liftIO . readIORef $ ref
  liftIO $ send address sub msg

-- * Application

data App = App {
    _sessionManager :: Snaplet SessionManager
  , _auth           :: Snaplet (AuthManager App)
  , _serviceContext :: Snaplet SnapletServiceContext
  , _dictionaryContext :: Snaplet DictionaryContext
  , _sendEmailContext   :: Snaplet SendEmailContext
  , _fayContext     :: Snaplet Fay
  }

makeLenses ''App

