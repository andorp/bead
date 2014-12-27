{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Command.Fayax where

import           Data.ByteString.Char8 (ByteString)
import           Data.Data
import           Data.Maybe

import qualified Fay.Text as Text
import           Snap.Snaplet.Fay
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Bead.Shared.Command
import           Bead.View.LoggedInFilter
import           Bead.View.BeadContext
import           Bead.View.Pagelets

-- The command is received through ajax request and the answer of
-- that command is send as the response to the client.
class (Data c, Read c, Show (Answer c)) => Command c where
  type Answer c
  fayaxInteract :: c -> BeadHandler (HandlerResult (Answer c))

ping = FCPing ()

routeHandler :: FayaxCommand a -> (ByteString, BeadHandler ())
routeHandler f = (fayaxCmdValue $ route f, fayaxCmdValue $ handler f)

-- Combines a given Fay ajax handler with the logged in filter,
-- which checks if the user is logged in. The result of the combination
-- is a filter which throws an exception if the user is not logged in, thus
-- it is not allowed to run ajax request without any authentication.
-- The exception is supposed to be catched by the snap at a certain point, when the
-- final message is generated with the "User is not logged in." message
fayaxLoginFilter
  :: (Data f1, Read f1, Show f2)
  => (f1 -> BeadHandler (HandlerResult f2))
  -> (f1 -> BeadHandler f2)
fayaxLoginFilter handler x = do
  result <- userIsLoggedInFilter (handler x) outside onError
  return $ fromMaybe resultError result
    where
      outside = return ()
      onError = const $ return ()
      resultError = error "User is not logged in."

-- Abstract request handler, for the type instanciation
req :: (Command c) => c -> BeadHandler (Answer c)
req = fayaxLoginFilter fayaxInteract

pingc :: Ping -> BeadHandler Pong
pingc = req

instance Command Ping where
  type Answer Ping = Pong
  fayaxInteract p = do
    let pingmsg = Text.unpack $ pingmessage p
        pongmsg = Text.pack   $ concat ["PONG: ", pingmsg ]
    return . HSuccess $ Pong { pongmessage = pongmsg }

-- Route constants for the fayax commands
route :: FayaxCommand a -> FayaxCommand ByteString
route = fayaxCmdConsts
  "/fay/ping"

-- Associates a fayax command with a handler
handler
  :: FayaxCommand a
  -> FayaxCommand (BeadHandler ())
handler = fayaxCmdConsts
  (fayax pingc)

-- * HTML pagelets for the forms, these forms needs to be hooked at the Fay side as well

-- Input forms for the fayax commands
fayaxCommandForm :: FayaxCommand a -> Html
fayaxCommandForm f = do
  fayaxForm "fayform" "/fay/form" $ H.div $ H.p $ do
    H.input ! A.type_ "hidden" ! A.name "instance" ! A.value (fayaxInstance f)
    (fayaxInputs f)
    submitButton (fayaxCmdValue $ fayaxSubmitId f) (fayaxSubmitText f)
  where
    fayaxInstance = fayaxCmdValue . fayaxCmdConsts
      "Ping"

    fayaxSubmitText = fayaxCmdValue . fayaxCmdConsts
      "Ping"

    fayaxInputs = fayaxCmdValue . fayaxCmdConsts
      (H.input ! A.type_ "text" ! A.name "pingmessage" ! A.value "") -- should be the same as defined in the Ping type
