{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Bootstrap where

{-
Collection of bootstrap related pagelets.
-}

import           Data.String

import           Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5 as H hiding (map)
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import           Bead.View.Snap.Content (selectionWithDefAndAttr)

formGroup = H.div ! class_ "form-group"

-- | Creates a list group div, which can contain a various list group items
listGroup = H.div ! class_ "list-group"

-- | Creates a linked list group item with a route to point at, and a text to
-- display
listGroupLinkItem route text = H.a ! href (fromString route) ! class_ "list-group-item" $ text

-- | Creates a texted list group item
listGroupTextItem text = H.a ! href "#" ! class_ "list-group-item" $ fromString text

-- | Creates a badge that can be displayed in the list group
badge text = H.span ! class_ "badge" $ fromString text

-- | Creates a form control selection with the given parameter name, a selector
-- function which determines the selected value, and possible values
selection paramName selector values =
  formGroup $ selectionWithDefAndAttr
    paramName
    [class_ "combobox form-control", A.style "display:none", A.required ""]
    selector
    values

-- | Creates a submit button with a given name
submitButton nameValue =
  button ! type_ "submit"
         ! (name $ fromString nameValue)
         ! class_ "btn btn-block btn-default"

-- | Creates an entry that triggers the combobox creation after the page load
comboboxScript
  = script ! type_ "text/javascript" $ "//\n$(document).ready(function(){\n$('.combobox').combobox()\n});\n//"

-- | Creates a password input with the given name as id, a given label within a form-group control
passwordInput paramName labelText =
  formGroup $ do
    H.label ! for (fromString paramName) $ (fromString labelText)
    H.input ! class_ "form-control"
            ! type_ "password"
            ! required ""
            ! name (fromString paramName)
            ! A.id (fromString paramName)

-- | Creates a text input with the given name as id, a given label and a placeholder text
textInput paramName labelText placeholderText =
  formGroup $ do
    H.label ! for (fromString paramName) $ (fromString labelText)
    H.input ! class_ "form-control"
            ! type_ "text"
            ! A.required ""
            ! A.name (fromString paramName)
            ! A.id (fromString paramName)
            ! A.placeholder (fromString placeholderText)

-- | Creates a text area input with the given name as id, a given label
textArea paramName labelText html =
  formGroup $ do
    H.label ! for (fromString paramName) $ (fromString labelText)
    H.textarea ! class_ "form-control"
               ! A.required ""
               ! A.rows "20"
               ! A.id (fromString paramName)
               ! A.name (fromString paramName) $ html

-- | Creates a radio button group, with a given values and labels, the parameter name
-- as numbered ids. The first value is the primary active
radioButtonGroup paramName valuesAndLabel =
  H.div ! class_ "btn-group" $
    mapM_ button ([1..] `zip` valuesAndLabel)
  where
    button (n,(v,l)) =
      H.label ! class_ "btn btn-primary" $ do
        H.input ! type_ "radio"
                ! name (fromString paramName)
                ! A.id (fromString (paramName ++ show n))
                ! A.value (fromString v)
        fromString l

-- | Creates a bootstrap row
row = H.div ! class_ "row"

-- | Creates a bootstrap 12 column
colMd12 = H.div ! class_ "col-md-12"

-- | Creates a bootstrap page header
pageHeader = H.div ! class_ "page-header"

-- | Creates a bootstrap table
table = H.table ! class_ "table table-bordered table-condensed table-hover table-striped"
