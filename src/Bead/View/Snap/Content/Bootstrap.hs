{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Bootstrap where

{-
Collection of bootstrap related pagelets.
-}

import           Control.Monad (when)

import           Data.Data
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.String

import           Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5 as H hiding (map)
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import           Bead.View.Snap.Fay.JSON.ServerSide

-- | Represents the possible sizes of columns
newtype ColumnSize = ColumnSize Int
  deriving Eq

columnSize f (ColumnSize s) = f s

colSize1  = ColumnSize 1
colSize2  = ColumnSize 2
colSize3  = ColumnSize 3
colSize4  = ColumnSize 4
colSize5  = ColumnSize 5
colSize6  = ColumnSize 6
colSize7  = ColumnSize 7
colSize8  = ColumnSize 8
colSize9  = ColumnSize 9
colSize10 = ColumnSize 10
colSize11 = ColumnSize 11
colSize12 = ColumnSize 12

-- Returns the HTML class attribute value for the given column size
columnSizeClass = columnSize $ \size -> "col-md-" ++ show size

-- | Represents the possible offsets of columns
newtype ColumnOffset = ColumnOffset Int
  deriving Eq

columnOffset f (ColumnOffset s) = f s

colOffset1  = ColumnOffset 1
colOffset2  = ColumnOffset 2
colOffset3  = ColumnOffset 3
colOffset4  = ColumnOffset 4
colOffset5  = ColumnOffset 5
colOffset6  = ColumnOffset 6
colOffset7  = ColumnOffset 7
colOffset8  = ColumnOffset 8
colOffset9  = ColumnOffset 9
colOffset10 = ColumnOffset 10
colOffset11 = ColumnOffset 11
colOffset12 = ColumnOffset 12

-- Returns the HTML class attribute value for the given column offset
columnOffsetClass = columnOffset $ \offset -> "col-md-offset-" ++ show offset

container = H.div ! class_ "container"

footer = H.div ! class_ "navbar navbar-default navbar-fixed-bottom"

formGroup = H.div ! class_ "form-group"

-- | Creates a list group div, which can contain a various list group items
listGroup = H.div ! class_ "list-group"

-- | Creates and unordered list as a list group
unorderedListGroup = H.ul ! class_ "list-group"

-- | Creates a linked list group item with a route to point at, and a text to
-- display
listGroupLinkItem route text = H.a ! href (fromString route) ! class_ "list-group-item" $ text

-- | Creates a texted list group item
listGroupTextItem text = H.a ! href "#" ! class_ "list-group-item" $ fromString text

-- | Creates a badge that can be displayed in the list group
badge text = H.span ! class_ "badge" $ fromString text

-- | Creates a caret sign
caret = H.span ! class_ "caret" $ mempty

-- | Creates a justified button group
buttonGroupJustified = H.div ! class_ "btn-group btn-group-justified"

-- | Creates a button group
buttonGroup = H.div ! class_ "btn-group"

-- | Creates a button link with custom button attribute, a route to point
-- a title and a text to show
customButtonLink custom ref ttl text =
  a ! class_ (fromString ("btn " <> custom))
    ! customAttribute "role" "button"
    ! A.title (fromString ttl)
    ! href (fromString ref)
    $ (fromString text)

-- | Creates a button styled link
buttonLink ref text = customButtonLink "btn-default" ref "" text

-- | Creates a date time picker using a third party library and turns on if the on switch
-- is set to True
datetimePicker paramName date on =
  H.div ! class_ "input-group date"
        ! A.id (fromString paramName) $ do
    input ! class_ "form-control"
          ! name (fromString paramName)
          ! type_ "text"
          ! readonly ""
          ! required ""
          ! value (fromString date)
    H.span ! class_ "input-group-addon" $ H.span ! class_ "glyphicon glyphicon-calendar" $ mempty
    when on $ dateTimePickerScript paramName

dateTimePickerScript pickerId = script . fromString $ concat
  [ "$(function () {"
  ,   "$('#", pickerId, "').datetimepicker({"
  ,     "format: 'YYYY-MM-DD HH:mm:ss',"
  ,     "pick12HourFormat: false,"
  ,     "pickSeconds: true"
  ,   "});"
  , "});"
  ]

-- | Creates a dropdown button
dropdownButton text =
  button ! type_ "button"
         ! class_ "btn btn-default dropdown-toggle"
         ! dataAttribute "toggle" "dropdown"
         $ do (fromString text); caret

-- | Creates a list of dropdown menu items
dropdownMenu items = H.ul ! class_ "dropdown-menu" ! customAttribute "role" "menu" $ mapM_ li items

-- | Creates a dropdown from the items with the given text on the button
dropdown text items = buttonGroup $ do
  dropdownButton text
  dropdownMenu items

-- | Creates a paragrapth that represents a help block from a given text
helpBlock text = p ! class_ "help-block" $ fromString text

-- | Creates a form control selection with the given parameter name, a selector
-- function which determines the selected value, and possible values
selection paramName selector values =
  formGroup $ selectionPart
    paramName
    [class_ "combobox form-control", A.style "display:none", A.required ""]
    selector
    values

-- | Creates a form control selection with the given parameter name, a label, a selector
-- function which determines the selected value, and possible values
selectionWithLabel paramName labelText selector values = formGroup $ do
  labelFor paramName labelText
  selectionPart
    paramName
    [class_ "combobox form-control", A.style "display:none", A.required ""]
    selector
    values

-- | Creates a submit block button with a given name and the given text
submitButton nameValue text =
  button ! type_ "submit"
         ! (name $ fromString nameValue)
         ! class_ "btn btn-block btn-default"
         $ fromString text

-- | Creates a submit button with a given attrbute and a given text
submitButtonWithAttr attr text =
  button ! type_ "submit"
         ! class_ "btn btn-block btn-default"
         ! attr
         $ fromString text

-- | Creates a submit small button with a given name and the given text
smallSubmitButton nameValue text =
  button ! type_ "submit"
         ! (name $ fromString nameValue)
         ! class_ "btn btn-default"
         $ fromString text

-- | Turns the selection into combobox like selections
turnSelectionsOn
  = script ! type_ "text/javascript" $ "//\n$(document).ready(function(){\n$('.combobox').combobox()\n});\n//"

-- | Creates a password input with the given name as id, a given label within a form-group control
passwordInput paramName labelText =
  formGroup $ do
    labelFor paramName labelText
    H.input ! class_ "form-control"
            ! type_ "password"
            ! required ""
            ! name (fromString paramName)
            ! A.id (fromString paramName)

inputForFormControl = H.input ! class_ "form-control"

-- | Creates a text input field only with a defualt value
textInputFieldWithDefault paramName value =
    H.input ! class_ "form-control"
            ! type_ "text"
            ! A.required ""
            ! A.name (fromString paramName)
            ! A.id (fromString paramName)
            ! A.value (fromString value)

-- | Creates a text input with the given name as id, a given label and a placeholder text
textInput paramName labelText placeholderText =
  formGroup $ do
    labelFor paramName labelText
    H.input ! class_ "form-control"
            ! type_ "text"
            ! A.required ""
            ! A.name (fromString paramName)
            ! A.id (fromString paramName)
            ! A.placeholder (fromString placeholderText)

-- | Creates a text input with the given name as id, a given label and a default value
textInputWithDefault paramName labelText value =
  formGroup $ do
    labelFor paramName labelText
    textInputFieldWithDefault paramName value

-- | Creates a label for the given id and given text
labelFor name text =
  H.label ! for (fromString name) $ (fromString text)

-- | Creates a labeled text as a form group element
labeledText name value =
  formGroup $ do
    H.label $ fromString $ name
    H.span ! class_ "form-control" $ value

-- | Creates a text area input field with the given name as id, a given id
textAreaField paramName =
    H.textarea ! class_ "form-control"
               ! A.required ""
               ! A.rows "20"
               ! A.id (fromString paramName)
               ! A.name (fromString paramName)

-- | Creates a text area input with the given name as id, a given label
textArea paramName labelText html =
  formGroup $ do
    labelFor paramName labelText
    textAreaField paramName html

-- | Creates a text area input with the given name as id, a given label
utf8TextArea paramName labelText html =
  formGroup $ do
    labelFor paramName labelText
    textAreaField paramName ! A.acceptCharset "utf-8" $ html

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

-- | Creates a bootstrap column with the given offset
colMd size offset =
  H.div ! class_ (fromString $ concat [columnSizeClass size, " ", columnOffsetClass offset])

-- | Creates a bootstrap 12 column
colMd12 = H.div ! class_ "col-md-12"

-- | Creates a bootstrap 6 width column
colMd6 = H.div ! class_ "col-md-6"

-- | Creates a bootstrap raw with only one colMd12 column
rowColMd12 = row . colMd12

-- | Creates a boostrap row with a 4 sized column in the middle of the page
rowCol4Offset4 = row . colMd colSize4 colOffset4

-- | Creates a bootstrap page header
pageHeader = H.div ! class_ "page-header"

-- | Creates a bootstrap table
table = H.table ! class_ "table table-bordered table-condensed table-hover table-striped"

-- HTML helpers

optionTag :: String -> String -> Bool -> Html
optionTag value text False = H.option ! A.value (fromString value)                 $ fromString text
optionTag value text True  = H.option ! A.value (fromString value) ! A.selected "" $ fromString text

selectTag :: String -> Html -> Html
selectTag name =
    H.select ! A.id (fromString name)
             ! A.name (fromString name)
             ! A.required ""

-- Encodes the value to Fay JSON representation or throw an error for the given name
encode :: (Data a, Show a, IsString s) => String -> a -> s
encode name value = fromString $ fromMaybe (name ++ ": error encoding value") (encodeToFay value)

selectionPart :: (Show a, Data a) =>
  String -> [Attribute] -> (a -> Bool) -> [(a, String)] -> Html
selectionPart name attrs def = foldl (!) (selectTag name) attrs . mapM_ option
  where
    option (v,t) = optionTag (encode "selection" v) t (def v)

textCenter = A.class_ "text-center"
