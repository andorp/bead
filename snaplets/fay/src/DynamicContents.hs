{-# LANGUAGE EmptyDataDecls #-}
module DynamicContents where

{- FAY compiled module -}

import FFI
import Prelude
import Data.Data
import JQuery hiding (filter, validate)
import Fay.JQueryUI
import Fay.Text

import Bead.Domain.Shared.Evaluation
import Bead.View.Snap.Fay.Hooks
import Bead.View.Snap.Fay.HookIds
import Bead.View.Snap.Fay.JSON.ClientSide
import Bead.View.Snap.Validators

main :: Fay ()
main = addOnLoad onload

onload :: Fay ()
onload = do
  hookPercentageDiv evaluationPctHook
  hookEvaluationTypeForm createCourseHook
  hookEvaluationTypeForm createGroupHook
  hookDatetimePickerDiv startDateTimeHook
  hookDatetimePickerDiv endDateTimeHook
  connectStartEndDatePickers startDateTimeHook endDateTimeHook
  connectStartEndHours startDateTimeHook endDateTimeHook
  hookAssignmentForm (hookId assignmentForm) startDateTimeHook endDateTimeHook
  hookRegistrationForm
  hookSamePasswords (rFormId regFinalForm) (lcFieldName loginPassword) (lcFieldName regPasswordAgain)
  hookPasswordField (rFormId regFinalForm) (lcFieldName loginPassword)
  hookPasswordField (rFormId changePwdForm) (cpf oldPasswordField)
  hookSamePasswords (rFormId changePwdForm) (cpf newPasswordField) (cpf newPasswordAgainField)
  hookSamePasswords (rFormId setStudentPwdForm) (cpf studentNewPwdField) (cpf studentNewPwdAgainField)
  hookLargeComments


connectStartEndDatePickers :: DateTimePickerHook -> DateTimePickerHook -> Fay ()
connectStartEndDatePickers start end = void $ do
  startId <- select . cssId $ dtDatePickerId start
  endId <- select . cssId $ dtDatePickerId end
  existStart <- exists startId
  existEnd <- exists endId
  when (existStart && existEnd) $ do
    setMinDatePickers startId endId
    setMaxDatePickers startId endId
    putStrLn "Date pickers are connected"

connectStartEndHours :: DateTimePickerHook -> DateTimePickerHook -> Fay ()
connectStartEndHours start end = void $ do
  startDate <- select . cssId $ dtDatePickerId start
  startHour <- select . cssId $ dtHourPickerId start
  startMin <- select . cssId $ dtMinPickerId start
  endDate <- select . cssId $ dtDatePickerId end
  endHour <- select . cssId $ dtHourPickerId end
  endMin <- select . cssId $ dtMinPickerId end
  existStartDate <- exists startDate
  existStartHour <- exists startHour
  existStartMin  <- exists startMin
  existEndDate <- exists endDate
  existEndHour <- exists endHour
  existEndMin  <- exists endMin
  let existAll = and [ existStartDate, existStartHour, existStartMin
                     , existEndDate, existEndHour, existEndMin
                     ]
  when existAll $ do
    let times = do
          sd <- getVal startDate
          sh <- getVal startHour
          sm <- getVal startMin
          ed <- getVal endDate
          eh <- getVal endHour
          em <- getVal endMin
          return (sd,sh,sm,ed,eh,em)

        -- Check is the start date and the end date is the same
        -- The start hour and minute must be before than the end hour and min
        checkStart e = void $ do
          (sd,sh,sm,ed,eh,em) <- times
          when (sd == ed) $ do
            when (less eh sh) $ setVal eh startHour
            when (eh == sh && less em sm) $ setVal em startMin

        checkEnd e = void $ do
          (sd,sh,sm,ed,eh,em) <- times
          when (sd == ed) $ do
            when (less eh sh) $ setVal sh endHour
            when (eh == sh && less em sm) $ setVal sm endMin

    spinStop   checkStart startHour
    spinStop   checkStart startMin
    spinStop   checkEnd   endHour
    spinStop   checkEnd   endMin
    spinChange checkStart startHour
    spinChange checkStart startMin
    spinChange checkEnd   endHour
    spinChange checkEnd   endMin
    putStrLn "Synchronization of hours and mins is hooked"

-- Attaches a datepicker and hour and minute input fields
-- to the given hook
hookDatetimePickerDiv :: DateTimePickerHook -> Fay ()
hookDatetimePickerDiv hook = void $ do
  let pickerDiv = dtDivId $ hook
  div   <- select . cssId $ pickerDiv -- dtDivId $ hook
  existDiv <- exists div
  when existDiv $ do
    input <- select . cssId . dtHiddenInputId $ hook
    date  <- select . createDateInput $ dtDatePickerId hook
    datepicker date
    hour <- select . createTimeInput $ dtHourPickerId hook
    min <- select . createTimeInput $ dtMinPickerId hook
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
    putStrLn $ "Date picker " ++ pickerDiv ++ " is loaded."
  where
    createDateInput id = fromString ("<input id=\"" ++ id ++ "\" type=\"text\" size=\"10\" required readonly />")
    createTimeInput id = fromString ("<input id=\"" ++ id ++ "\" type=\"text\" size=\"2\" value=\"0\"/>")
    newLine = fromString "<br/>"

    datetime d h m = fromString $ (unpack d) ++ " " ++ (twoDigits (unpack h)) ++ ":" ++ (twoDigits (unpack m)) ++ ":00"

    copy from to = do
      x <- getVal from
      setVal x to

twoDigits [d] = ['0',d]
twoDigits ds  = ds

hookPercentageDiv :: PercentageHook -> Fay ()
hookPercentageDiv hook = void $ do
  div <- select . cssId . ptDivId $ hook
  existDiv <- exists div
  when existDiv $ do
    input <- select . cssId . ptHiddenInputId $ hook
    checkboxMsg <- (select . cssId $ hookId evCommentOnlyText) >>= getVal
    commentCheckbox <- select (fromString $ concat [ "<input type=\"checkbox\">", unpack checkboxMsg, "</input>" ])
    appendTo div commentCheckbox
    select (fromString "<br/>") >>= appendTo div
    pctInput <- select (fromString "<input type=\"text\" size=\"3\" required />")
    appendTo div pctInput
    pctSymbol <- select (fromString "<span class=\"evtremoveable\">&#37;</span>")
    appendTo div pctSymbol
    let changeHiddenInput e = void $ do
          t <- targetElement e
          val <- getVal t
          setVal (fromString . result . double . unpack $ val) input
    let commentCheckboxChanged e = void $ do
          t <- target e
          c <- checked t
          if c
            then do
              hide Instantly pctInput
              hide Instantly pctSymbol
              setVal (fromString comment) input
            else do
              unhide pctInput
              unhide pctSymbol
              val <- getVal pctInput
              setVal (fromString . result . double . unpack $ val) input
    change commentCheckboxChanged commentCheckbox
    numberField pctInput 0 100
    pctSpinner changeHiddenInput pctInput
    putStrLn "Percentage div is loaded."
  where
    double "100" = "1.0"
    double ds    = "0." ++ twoDigits ds

    -- Converts a string percentage into a JSON EvalOrComment percentage value representation
    result :: String -> String
    result = evalOrCommentJson . EvCmtResult . percentageResult . parseDouble

    comment :: String
    comment = evalOrCommentJson EvCmtComment

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
  existForm <- exists form
  when existForm $ do
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
  existForm <- exists form
  when existForm $ do
    pwd <- select $ cssId password
    let validatorPwd = do
          messages <- select . cssClass $ removable
          remove messages
          validateField isPassword pwd pwd removable
    onSubmit validatorPwd form
    putStrLn "Password field is loaded."
  where
    removable = "password_error"

-- Checks if the form has a password and a password again field
-- and hook that the two values must be the same for submit event.
hookSamePasswords :: String -> String -> String -> Fay ()
hookSamePasswords formId password1 password2 = void $ do
  form      <- select . cssId $ formId
  existForm <- exists form
  when existForm $ do
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
              span <- makeMessage removable "A jelszavak nem egyeznek!"
              after span pwdAgain
              return False
    onSubmit validator form
    putStrLn "Same password fields are loaded."
  where
    removable = "same_password_error"

hookRegistrationForm :: Fay ()
hookRegistrationForm = void $ do
  regForm <- select . cssId . rFormId $ regForm
  existForm <- exists regForm
  when existForm $ do
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
    putStrLn "Registration form is loaded."
  where
    removable = "regremovable"

hookEvaluationTypeForm :: EvaluationHook -> Fay ()
hookEvaluationTypeForm hook = do
  let formId = evFormId hook
  form <- select . cssId $ formId
  existForm <- exists form
  when existForm $ do
    selection <- select . cssId . evSelectionId $ hook
    change (changeFormContent form) selection
    setDefaultEvalConfig form selection
    putStrLn $ "Evaluation form " ++ formId ++ " is loaded."

  where
    setDefaultEvalConfig :: JQuery -> JQuery -> Fay ()
    setDefaultEvalConfig form selection = do
      value <- decodeEvalType <$> firstOptionValue selection
      case value of
        (BinEval _) -> setEvaluationConfig binaryConfig
        (PctEval _) -> addPercentageField form

    changeFormContent :: JQuery -> Event -> Fay ()
    changeFormContent form e = void $ do
      t <- target e
      v <- decodeEvalType <$> selectedValue t
      findSelector (fromString ".evtremoveable") form >>= remove
      case v of
        (BinEval _) -> setEvaluationConfig binaryConfig
        (PctEval _) -> addPercentageField form

    addPercentageField :: JQuery -> Fay ()
    addPercentageField form = void $ do
      div <- findSelector (cssId . evSelectionDivId $ hook) form
      msgSpan <- select . cssId $ evHelpMessageId hook
      msgValue <- getText msgSpan
      msg <- select (fromString ("<span class=\"evtremoveable\">" ++ (unpack msgValue) ++ "</span><br class=\"evtremoveable\">"))
      appendTo div msg
      pctInput <- select (fromString "<input type=\"text\" id=\"percentage\" class=\"evtremoveable\" size=\"3\" required />")
      appendTo div pctInput
      select (fromString "<span class=\"evtremoveable\">&#37;</span>") >>= appendTo div
      numberField pctInput 0 100
      pctSpinner setEvalLimit pctInput
      change setEvalLimit pctInput
      setEvaluationConfig (percentageConfig 0.0)

    setEvalLimit :: Event -> Fay ()
    setEvalLimit e = do
      t <- targetElement e
      v <- getVal t
      let pct = case unpack v of
                  "100" -> "1.0"
                  ds    -> "0." ++ (twoDigits ds)
      setEvaluationConfig (percentageConfig $ parseDouble pct)

    setEvaluationConfig :: EvConfig -> Fay ()
    setEvaluationConfig c =
      void $ select (cssId . evHiddenValueId $ hook) >>=
             setVal (fromString . evConfigJson $ c)

hookLargeComments :: Fay ()
hookLargeComments = void $ do
  moreButton <- select (fromString "a.morebutton")
  click toggle moreButton
  where
    a_morebutton = fromString ("a." ++ hookClass moreButtonClass)
    pre_more     = fromString ("pre." ++ hookClass moreClass)
    morebutton   = fromString (hookClass moreButtonClass)
    lessbutton   = fromString (hookClass lessButtonClass)
    span_seeless = fromString ("span." ++ hookClass seeLessClass)
    span_seemore = fromString ("span." ++ hookClass seeMoreClass)

    toggle :: Event -> Fay ()
    toggle e = void $ do
      seeMore <- (target e) >>= selectElement
      full <- (nextSelector pre_more seeMore >>= first)
      slideToggle full
      preview <- (prevSelector pre_more seeMore >>= first)
      slideToggle preview
      seeMoreClass <- hasClass morebutton seeMore
      if (seeMoreClass)
        then do seeLessText <- (prevAllSelector span_seeless seeMore) >>= getText
                setText     seeLessText seeMore
                removeClass morebutton  seeMore
                addClass    lessbutton  seeMore
        else do seeMoreText <- (prevAllSelector span_seemore seeMore) >>= getText
                setText     seeMoreText seeMore
                removeClass lessbutton  seeMore
                addClass    morebutton  seeMore

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
targetElement e = (target e) >>= selectElement

exists :: JQuery -> Fay Bool
exists = ffi "(%1.length != 0)"

-- * Javascript helpers

selectedValue :: Element -> Fay String
selectedValue = ffi "%1.options[%1.selectedIndex].value"

-- Checks if the given jquery represents a checkbox, and
-- returns if it is checked or not. If the jquery is not a checkbox throws
-- an exception
checked :: Element -> Fay Bool
checked = ffi "%1.checked"

firstOptionValue :: JQuery -> Fay String
firstOptionValue selection = do
  findSelector (fromString "option") selection >>= first >>= getVal >>= (return . unpack)

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

-- Returns true if the first text is greater in js than the second
greater :: Text -> Text -> Bool
greater = ffi "(%1) > (%2)"

-- Returns true if the first text is less in js than the second
less :: Text -> Text -> Bool
less = ffi "(%1) < (%2)"

