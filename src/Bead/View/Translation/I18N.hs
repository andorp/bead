module Bead.View.Translation.I18N where

import Text.Printf (printf)
import Bead.View.Translation.Base

-- The I18N is a mapping from a given translation key
-- to the actual translation of the message
type I18N = Translation String -> String

-- | The Translation Message represents a message that
-- can be rendered out the the UI, the message could
-- be a normal message or a parametrized one
data TransMsg
  = TransMsg (Translation String)
  | TransPrmMsg (Translation String) String
  | TransPrm2Msg (Translation String) String String
  | TransPrm3Msg (Translation String) String String String
  deriving (Show)

-- Template method for TransMsg function
transMsgCata
  transMsg     f
  transPrmMsg  g
  transPrm2Msg h
  transPrm3Msg i
  tm = case tm of
    TransMsg     t          -> transMsg     (f t)
    TransPrmMsg  t p1       -> transPrmMsg  (g t) p1
    TransPrm2Msg t p1 p2    -> transPrm2Msg (h t) p1 p2
    TransPrm3Msg t p1 p2 p3 -> transPrm3Msg (i t) p1 p2 p3

-- Translate the parametrized message with the given localization
translateMessage :: I18N -> TransMsg -> String
translateMessage i18n = transMsgCata
  id     i18n
  printf i18n
  printf i18n
  printf i18n
