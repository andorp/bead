{-# LANGUAGE EmptyDataDecls #-}
module DynamicContents where

{- FAY compiled module -}

import FFI
import Prelude
import Data.Data
import JQuery hiding (filter, validate)
import Fay.JQueryUI

import Bead.Domain.Shared.Evaulation
import Bead.View.Snap.Fay.Hooks
import Bead.View.Snap.Fay.HookIds
import Bead.View.Snap.Fay.JSON.ClientSide
import Bead.View.Snap.Validators

main :: Fay ()
main = addOnLoad onload

onload :: Fay ()
onload = do
  hookEvaulationTypeForm createCourseHook
  hookEvaulationTypeForm createGroupHook
  hookDatetimePickerDiv startDateTimeHook
  hookDatetimePickerDiv endDateTimeHook
  hookPercentageDiv evaulationPctHook pctValue
  hookRegistrationForm

pctValue :: String -> String
pctValue = toEvResultJSON . percentageResult . parseDouble

hookDatetimePickerDiv :: DateTimePickerHook -> Fay ()
hookDatetimePickerDiv hook = void $ do
  div   <- select . cssId . dtDivId $ hook
  input <- select . cssId . dtHiddenInputId $ hook
  date  <- select createDateInput
  datepicker date
  hour <- select createTimeInput
  min <- select createTimeInput
  br  <- select newLine
  appendTo div date
  appendTo div br
  appendTo div hour
  appendTo div min
  numberField hour 0 23
  numberField min  0 59
  let createDateTime e = void $ do
        d <- getVal date
        h <- getVal hour
        m <- getVal min
        setVal (datetime d h m) input
  hourSpinner createDateTime hour
  minuteSpinner createDateTime min
  change createDateTime date
  change createDateTime hour
  change createDateTime min
  where
    createDateInput = "<input type=\"text\" size=\"10\" required readonly />"
    createTimeInput = "<input type=\"text\" size=\"2\" value=\"0\"/>"
    newLine = "<br/>"

    datetime d h m = d ++ " " ++ (twoDigits h) ++ ":" ++ (twoDigits m) ++ ":00"

twoDigits [d] = ['0',d]
twoDigits ds  = ds

hookPercentageDiv :: PercentageHook -> (String -> String) -> Fay ()
hookPercentageDiv hook f = void $ do
  div <- select . cssId . ptDivId $ hook
  input <- select. cssId . ptHiddenInputId $ hook
  pctInput <- select "<input type=\"text\" size=\"3\" required />"
  appendTo div pctInput
  select "<span class=\"evtremoveable\">&#37;</span>" >>= appendTo div
  let changeHiddenInput e = void $ do
        t <- targetElement e
        val <- getVal t
        setVal (f . double $ val) input
  numberField pctInput 0 100
  pctSpinner changeHiddenInput pctInput
  where
    double "100" = "1.0"
    double n = "0." ++ twoDigits n

numberField :: JQuery -> Int -> Int -> Fay ()
numberField i min max = do
  setVal (printInt min) i
  flip keyup i $ \e -> void $ do
    t <- targetElement e
    val <- getVal t
    setVal (filter isDigit val) t
  flip change i $ \e -> void $ do
    t <- targetElement e
    val <- getVal t
    case val of
      [] -> void $ setVal "0" t
      v  -> do let x = parseInt v
               when (x <  min) $ setVal (show min) t
               when (x >= max) $ setVal (show max) t

hookRegistrationForm :: Fay ()
hookRegistrationForm = void $ do
  regForm <- select . cssId . rFormId $ regForm
  uname <- select . cssId . lcFieldName $ loginUsername
  pwd   <- select . cssId . lcFieldName $ loginPassword
  email <- select . cssId . rFieldName $ regEmailAddress
  fname <- select . cssId . rFieldName $ regFullName

  let validator = do
        messages <- select . cssClass $ removable
        remove messages
        andM
          [ validateField isUsername uname
          , validateField isPassword pwd
          , validateField isEmailAddress email
          ]

  onSubmit validator regForm
  where
    validateField :: FieldValidator -> JQuery -> Fay Bool
    validateField f e = do
      v <- getVal e
      validate f v
        (return True)
        (\m -> do span <- makeMessage m
                  after span e
                  return False)

    removable = "regremovable"
    makeMessage msg = select (
      "<br><snap style=\"font-size: smaller\" class=\"" ++
      removable ++ "\">"
      ++ msg ++
      "</span>")

hookEvaulationTypeForm :: EvaulationHook -> Fay ()
hookEvaulationTypeForm hook = do
  form <- select . cssId . evFormId $ hook
  selection <- select . cssId . evSelectionId $ hook
  change (changeFormContent form) selection

  where
    changeFormContent :: JQuery -> Event -> Fay ()
    changeFormContent form e = void $ do
      t <- target e
      v <- decodeEvalType <$> selectedValue t
      findSelector ".evtremoveable" form >>= remove
      case v of
        (BinEval _) -> setEvaulationValue (BinEval ())
        (PctEval _) -> addPercentageField form

    addPercentageField :: JQuery -> Fay ()
    addPercentageField form = void $ do
      pctInput <- select "<input type=\"text\" id=\"percentage\" class=\"evtremoveable\" size=\"3\" required />"
      div <- findSelector (cssId . evSelectionDivId $ hook) form
      appendTo div pctInput
      select "<span class=\"evtremoveable\">&#37;</span>" >>= appendTo div
      numberField pctInput 0 100
      pctSpinner setEvalLimit pctInput
      change setEvalLimit pctInput

    setEvalLimit :: Event -> Fay ()
    setEvalLimit e = do
      t <- targetElement e
      v <- getVal t
      let pct = "0." ++ (twoDigits v)
      setEvaulationValue (PctEval pct)

    setEvaulationValue :: EvaulationData () String -> Fay ()
    setEvaulationValue c = void $ select (cssId . evHiddenValueId $ hook) >>= setVal (value c)
      where
        value (BinEval ()) = "BinEval ()"
        value (PctEval d) = "PctEval " ++ d

cssId :: String -> String
cssId i = '#':i

cssClass :: String -> String
cssClass c = '.':c

-- * Helpers

void :: Fay a -> Fay ()
void f = f >> return ()

(<$>) :: (a -> b) -> Fay a -> Fay b
f <$> m = m >>= (return . f)

andM :: [Fay Bool] -> Fay Bool
andM fs = and <$> (sequence fs)

-- * JQuery helpers

sValue :: JQuery -> Fay Int
sValue = ffi "%1.spinner(\"value\")"

targetElement :: Event -> Fay JQuery
targetElement e = target e >>= selectElement

-- * Javascript helpers

selectedValue :: Element -> Fay String
selectedValue = ffi "%1.options[%1.selectedIndex].value"

addOnLoad :: Fay a -> Fay ()
addOnLoad = ffi "window.addEventListener(\"load\", %1)"

value :: Element -> Fay String
value = ffi "%1.value"

parseInt :: String -> Int
parseInt = ffi "parseInt(%1)"

parseDouble :: String -> Double
parseDouble = ffi "parseFloat(%1)"

printInt :: Int -> String
printInt = ffi "%1.toString()"
