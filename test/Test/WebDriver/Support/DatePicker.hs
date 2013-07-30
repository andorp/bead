module Test.WebDriver.Support.DatePicker (
    DatePicker
  , datepickerFold
  , datepicker
  , selectDay
  , prevMonth
  , nextMonth
  ) where

import Control.Monad ((>=>))
import Data.String (fromString)

import Test.WebDriver
import Test.WebDriver.Classes

import Test.WebDriver.Support.Table

-- Represents a day of a month (1..28,29,30,31)
type Day = Int

-- Represents a month of a year (1..12)
type Month = Int
-- E.g January - 1, .. December - 12

-- Represents a JQueryUI datepicker element on the page
data DatePicker = DatePicker {
    rootElement :: Element -- The root div element of the date picker
  }

datepickerFold :: (Element -> a) -> DatePicker -> a
datepickerFold f = f . rootElement

-- Creates a datepicker from an element.
-- Produces Just DatePicker if the element has a datepicker strucutre
-- otherwise Nothing
datepicker :: (WebDriver wd) => Element -> wd (Maybe DatePicker)
datepicker e = do
  calendar <- findElemsFrom e (ByClass . fromString $ "ui-datepicker-header")
  header   <- findElemsFrom e (ByClass . fromString $ "ui-datepicker-calendar")
  case (calendar, header) of
    ([c],[h]) -> return . Just . DatePicker $ e
    _         -> return Nothing

-- Selects the calendar table of the date picker element
calendarTable :: (WebDriver wd) => DatePicker -> wd Element
calendarTable = datepickerFold (find (ByClass . fromString $ "ui-datepicker-calendar"))

-- Selects a day in the actual month
selectDay :: (WebDriver wd) => Day -> DatePicker -> wd ()
selectDay d p = calendarTable p >>= (find (ByLinkText . fromString . show $ d) >=> click)

-- Selects the next month
nextMonth :: (WebDriver wd) => DatePicker -> wd ()
nextMonth = datepickerFold (find (ByClass . fromString $ "ui-datepicker-next") >=> click)

-- Selects the previous month
prevMonth :: (WebDriver wd) => DatePicker -> wd ()
prevMonth = datepickerFold (find (ByClass . fromString $ "ui-datepicker-prev") >=> click)

find selector element = findElemFrom element selector
