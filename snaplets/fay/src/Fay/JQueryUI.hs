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
