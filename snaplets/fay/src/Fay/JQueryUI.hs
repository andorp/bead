module Fay.JQueryUI where

import FFI
import Prelude
import JQuery

datepicker :: JQuery -> Fay JQuery
datepicker = ffi "%1.datepicker({ dateFormat: \"yy-mm-dd\"})"

hourSpinner :: JQuery -> Fay JQuery
hourSpinner = ffi "%1.spinner({min:0, max:23})"

minuteSpinner :: JQuery -> Fay JQuery
minuteSpinner = ffi "%1.spinner({min:0, max:59})"

