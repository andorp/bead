module Fay.JQueryUI where

import FFI
import Prelude
import JQuery

datepicker :: JQuery -> Fay JQuery
datepicker = ffi "%1.datepicker({dateFormat: \"yy-mm-dd\", constrainInput: true})"

hourSpinner :: (Event -> Fay ()) -> JQuery -> Fay JQuery
hourSpinner = ffi "%2.spinner({min:0, max:23, stop: %1})"

minuteSpinner :: (Event -> Fay ()) -> JQuery -> Fay JQuery
minuteSpinner = ffi "%2.spinner({min:0, max:59, stop: %1})"

pctSpinner :: (Event -> Fay ()) -> JQuery -> Fay JQuery
pctSpinner = ffi "%2.spinner({min:0, max: 100, stop: %1})"

-- Represents a Spinner on the page
newtype Spinner = Spinner JQuery

-- Template for spinner functions
spinner f (Spinner locator) = f locator

-- Template for spinner functions
withSpinner s f = spinner f s

-- Attaches an event handler what is triggered when the spinner is stopped
spinnerSpinStop :: (Event -> Fay ()) -> Spinner -> Fay ()
spinnerSpinStop handler = spinner (spinStop handler)

-- Attaches an event handler what is triggered when the value of the spinner
-- has changes and the input is no longer focused
spinnerSpinChange :: (Event -> Fay ()) -> Spinner -> Fay ()
spinnerSpinChange handler = spinner (spinChange handler)

-- Set the spinner value
setSpinnerValue :: Int -> JQuery -> Fay ()
setSpinnerValue = ffi "%2.spinner('value', %1)"

enableSpinner :: Spinner -> Fay ()
enableSpinner = spinner enableSpinner'

enableSpinner' :: JQuery -> Fay ()
enableSpinner' = ffi "%1.spinner('enable')"

disableSpinner :: Spinner -> Fay ()
disableSpinner = spinner disableSpinner'

disableSpinner' :: JQuery -> Fay ()
disableSpinner' = ffi "%1.spinner('disable')"

-- Attaches an event handler what is triggered when the spinner is stopped
spinStop :: (Event -> Fay ()) -> JQuery -> Fay ()
spinStop = ffi "(%2).on('spinstop', (%1))"

-- Attaches an event handler what is triggered when the value of the spinner
-- has changes and the input is no longer focused
spinChange :: (Event -> Fay ()) -> JQuery -> Fay ()
spinChange = ffi "(%2).on('spinchange', (%1))"

-- Sets the seconds date picker's min date to the selected date in the first date picker
setMinDatePickers :: JQuery -> JQuery -> Fay ()
setMinDatePickers = ffi "(%1).datepicker('option', 'onClose', function(selectedDate) { (%2).datepicker('option', 'minDate', selectedDate); })"

-- Sets the first date picker's max date to the selected date in the second date picker
setMaxDatePickers :: JQuery -> JQuery -> Fay ()
setMaxDatePickers = ffi "(%2).datepicker('option', 'onClose', function(selectDate) { (%1).datepicker('option', 'maxDate', selectedDate); })"

slideToggle :: JQuery -> Fay ()
slideToggle = ffi "(%1).slideToggle(\"fast\")"

