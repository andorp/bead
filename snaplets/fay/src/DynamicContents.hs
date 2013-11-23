{-# LANGUAGE EmptyDataDecls #-}
module DynamicContents where

{- FAY compiled module -}

import FFI
import Prelude
import Data.Data
import JQuery hiding (filter, validate)
import Fay.JQueryUI
import Fay.Text

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
  hookAssignmentForm (hookId assignmentForm) startDateTimeHook endDateTimeHook
  hookPercentageDiv evaulationPctHook pctValue
  hookRegistrationForm
  hookSamePasswords (rFormId regFinalForm) (lcFieldName loginPassword) (lcFieldName regPasswordAgain)
  hookPasswordField (rFormId regFinalForm) (lcFieldName loginPassword)
  hookPasswordField (rFormId changePwdForm) (cpf oldPasswordField)
  hookSamePasswords (rFormId changePwdForm) (cpf newPasswordField) (cpf newPasswordAgainField)
  hookSamePasswords (rFormId setStudentPwdForm) (cpf studentNewPwdField) (cpf studentNewPwdAgainField)

pctValue :: String -> String
pctValue = toEvResultJSON . percentageResult . parseDouble

-- Attaches a datepicker and hour and minute input fields
-- to the given hook
hookDatetimePickerDiv :: DateTimePickerHook -> Fay ()
hookDatetimePickerDiv hook = void $ do
  div   <- select . cssId . dtDivId $ hook
  input <- select . cssId . dtHiddenInputId $ hook
  date  <- select . createDateInput $ dtDatePickerId hook
  datepicker date
  hour <- select createTimeInput
  min <- select createTimeInput
  br  <- select newLine
  appendTo div br
  appendTo div date
  appendTo div br
  appendTo div hour
  appendTo div min
  numberField hour 0 23
  numberField min  0 59
  defDate <- select . cssId . dtDefaultDate $ hook
  defHour <- select . cssId . dtDefaultHour $ hook
  defMin  <- select . cssId . dtDefaultMin  $ hook
  copy defDate date
  copy defHour hour
  copy defMin  min
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
    createDateInput id = fromString ("<input id=\"" ++ id  ++ "\" type=\"text\" size=\"10\" required readonly />")
    createTimeInput = fromString "<input type=\"text\" size=\"2\" value=\"0\"/>"
    newLine = fromString "<br/>"

    datetime d h m = fromString $ (unpack d) ++ " " ++ (twoDigits (unpack h)) ++ ":" ++ (twoDigits (unpack m)) ++ ":00"

    copy from to = do
      x <- getVal from
      setVal x to

twoDigits [d] = ['0',d]
twoDigits ds  = ds

hookPercentageDiv :: PercentageHook -> (String -> String) -> Fay ()
hookPercentageDiv hook f = void $ do
  div <- select . cssId . ptDivId $ hook
  input <- select. cssId . ptHiddenInputId $ hook
  pctInput <- select (fromString "<input type=\"text\" size=\"3\" required />")
  appendTo div pctInput
  select (fromString "<span class=\"evtremoveable\">&#37;</span>") >>= appendTo div
  let changeHiddenInput e = void $ do
        t <- targetElement e
        val <- getVal t
        setVal (fromString . f . double . unpack $ val) input
  numberField pctInput 0 100
  pctSpinner changeHiddenInput pctInput
  where
    double "100" = "1.0"
    double n = "0." ++ twoDigits n

numberField :: JQuery -> Int -> Int -> Fay ()
numberField i min max = do
  setVal (fromString . printInt $ min) i
  flip keyup i $ \e -> void $ do
    t <- targetElement e
    val <- getVal t
    setVal (fromString $ filter isDigit (unpack val)) t
  flip change i $ \e -> void $ do
    t <- targetElement e
    val <- getVal t
    case (unpack val) of
      [] -> void $ setVal (fromString "0") t
      v  -> do let x = parseInt v
               when (x <  min) $ setVal (fromString . show $ min) t
               when (x >= max) $ setVal (fromString . show $ max) t

makeMessage removable msg = select . fromString $ (
  "<br class=\"" ++ removable ++ "\"><snap style=\"font-size: smaller;color: red\" class=\"" ++
  removable ++ "\">"
  ++ msg ++
  "</span>")

-- Validate a field with a given field validator, and places
-- a message with a given class, after the given place
validateField :: FieldValidator -> JQuery -> JQuery -> String -> Fay Bool
validateField validator field place removable = do
  v <- getVal field
  validate validator (unpack v)
    (return True)
    (\m -> do span <- makeMessage removable (message validator)
              after span place
              return False)

-- Hooks the assignment form with a validator, that runs on the submit of the
-- form. The two dates must be selected properly, otherwise the form
-- can't be submitted.
hookAssignmentForm :: String -> DateTimePickerHook -> DateTimePickerHook -> Fay ()
hookAssignmentForm formId startHook endHook = void $ do
  form <- select $ cssId formId
  start <- select . cssId $ dtHiddenInputId startHook
  end <- select . cssId $ dtHiddenInputId endHook
  startErrMsgPos <- select . cssId $ dtDatePickerId startHook
  endErrMsgPos <- select . cssId $ dtDatePickerId endHook
  let validateForm = do
        messages <- select . cssClass $ removeable
        remove messages
        start <- validateField isDateTime start startErrMsgPos removeable
        end   <- validateField isDateTime end   endErrMsgPos   removeable
        return (start && end)
  onSubmit validateForm form
  where
    removeable = "datetimeerror"

-- Checks if the page contains password field
-- and hook it with the validator for submit event.
hookPasswordField :: String -> String -> Fay ()
hookPasswordField formId password = void $ do
  form <- select . cssId $ formId
  pwd <- select $ cssId password
  let validatorPwd = do
        messages <- select . cssClass $ removable
        remove messages
        validateField isPassword pwd pwd removable
  onSubmit validatorPwd form
  where
    removable = "password_error"

-- Checks if the form has a password and a password again field
-- and hook that the two values must be the same for submit event.
hookSamePasswords :: String -> String -> String -> Fay ()
hookSamePasswords formId password1 password2 = void $ do
  form     <- select . cssId $ formId
  pwd      <- select $ cssId password1
  pwdAgain <- select $ cssId password2
  let validator = do
        messages <- select . cssClass $ removable
        remove messages
        password      <- getVal pwd
        passwordAgain <- getVal pwdAgain
        case (password == passwordAgain) of
          True -> return True
          False -> do
            span <- makeMessage removable "Given passwords are differents"
            after span pwdAgain
            return False
  onSubmit validator form
  putStrLn "There were some password fields hooked."
  where
    removable = "same_password_error"

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
          [ validateField isUsername     uname uname removable
          , validateField isEmailAddress email email removable
          ]

  onSubmit validator regForm
  where
    removable = "regremovable"

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
      findSelector (fromString ".evtremoveable") form >>= remove
      case v of
        (BinEval _) -> setEvaulationValue (BinEval ())
        (PctEval _) -> addPercentageField form

    addPercentageField :: JQuery -> Fay ()
    addPercentageField form = void $ do
      pctInput <- select (fromString "<input type=\"text\" id=\"percentage\" class=\"evtremoveable\" size=\"3\" required />")
      div <- findSelector (cssId . evSelectionDivId $ hook) form
      appendTo div pctInput
      select (fromString "<span class=\"evtremoveable\">&#37;</span>") >>= appendTo div
      numberField pctInput 0 100
      pctSpinner setEvalLimit pctInput
      change setEvalLimit pctInput

    setEvalLimit :: Event -> Fay ()
    setEvalLimit e = do
      t <- targetElement e
      v <- getVal t
      let pct = "0." ++ (twoDigits (unpack v))
      setEvaulationValue (PctEval pct)

    setEvaulationValue :: EvaulationData () String -> Fay ()
    setEvaulationValue c = void $ select (cssId . evHiddenValueId $ hook) >>= setVal (fromString . value $ c)
      where
        value (BinEval ()) = "BinEval ()"
        value (PctEval d) = "PctEval " ++ d

cssId :: String -> Text
cssId i = fromString ('#':i)

cssClass :: String -> Text
cssClass c = fromString ('.':c)

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
