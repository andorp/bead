{-# LANGUAGE EmptyDataDecls #-}
module DynamicContents where

{- FAY compiled module -}

import FFI
import Prelude
import JQuery
import Fay.JQueryUI

import Bead.Domain.Shared
import Bead.View.Snap.Fay.Hooks
import Bead.View.Snap.Fay.HookIds

main :: Fay ()
main = addOnLoad onload

onload :: Fay ()
onload = do
  hookEvaulationTypeForm createCourseHook
  hookEvaulationTypeForm createGroupHook
  attachDatePickers

attachDatePickers :: Fay ()
attachDatePickers = void $ do
  (select . cssClass . hookClass $ datePickerClass)  >>= datepicker
  (select . cssClass . hookClass $ hourSpinnerClass) >>= hourSpinner
  (select . cssClass . hookClass $ minuteSpinnerClass) >>= minuteSpinner

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
      textInput <- select "<input type=\"text\" id=\"percentage\" class=\"evtremoveable\"/>"
      div <- findSelector (cssId . evSelectionDivId $ hook) form
      appendTo div textInput
      change setEvalLimit textInput

    setEvalLimit :: Event -> Fay ()
    setEvalLimit e = target e >>= value >>= (setEvaulationValue . PctEval)

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

-- * Javascript helpers

selectedValue :: Element -> Fay String
selectedValue = ffi "%1.options[%1.selectedIndex].value"

addOnLoad :: Fay a -> Fay ()
addOnLoad = ffi "window.addEventListener(\"load\", %1)"

value :: Element -> Fay String
value = ffi "%1.value"
