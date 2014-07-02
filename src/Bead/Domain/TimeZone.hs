module Bead.Domain.TimeZone (
    TimeZoneConverter(..)
  , createTimeZoneConverter
  , zoneInfoToLocalTime'
  , zoneInfoToUTCTime'
  , zoneInfoToTimeZone'
  , zoneInfoToLocalTimeSafe
  , zoneInfoToUTCTimeSafe
  , zoneInfoToTimeZoneSafe

  -- Backward compatibility
  , utcZoneInfo
  , cetZoneInfo
  ) where

{-
TimeZones are handled by the olson library.
-}

import           Control.Exception
import           Control.Monad
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Time hiding (timeZoneName)
import qualified Data.Time.LocalTime as Local (utc)
import           System.FilePath
import           System.Directory

import           Data.Time.LocalTime.TimeZone.Olson
import           Data.Time.LocalTime.TimeZone.Series

import           Bead.Domain.Entities (TimeZoneName(..), timeZoneName)

utcZoneInfo = TimeZoneName "UTC"
cetZoneInfo = TimeZoneName "CET"

-- | Represents the conversational functions
-- between times given in UTC times and LocalTimes
-- and functions to gen the name for the timezome
data TimeZoneConverter = TimeZoneConverter {
    zoneInfoToLocalTime :: TimeZoneName -> UTCTime -> Maybe LocalTime
    -- ^ Convert the given UTC time to a LocalTime of the given TimeZoneName
  , zoneInfoToUTCTime   :: TimeZoneName -> LocalTime -> Maybe UTCTime
    -- ^ Convert the given LocalTime of the given TimeZoneName to UTC time
  , zoneInfoToTimeZone  :: TimeZoneName -> UTCTime -> Maybe TimeZone
    -- ^ Calculates the actual time zone of the given UTC time for the
    -- given TimeZoneName
  , zoneInfos :: [TimeZoneName]
    -- ^ List of the Time Zones found for the TimeZoneConverter
  }
-- If the TimeZoneName is not found all the functions calculates Nothing

errTimeZoneName = timeZoneName (error . ("Zone Info was not loaded" ++))

-- Throws an exception if the zone is not found
zoneInfoToLocalTime' tzc zi utc = fromMaybe (errTimeZoneName zi) (zoneInfoToLocalTime tzc zi utc)
zoneInfoToUTCTime' tzc zi lcl   = fromMaybe (errTimeZoneName zi) (zoneInfoToUTCTime tzc zi lcl)
zoneInfoToTimeZone' tzc zi utc  = fromMaybe (errTimeZoneName zi) (zoneInfoToTimeZone tzc zi utc)

-- Returns the UTC time if the TimeZoneName is not found
zoneInfoToLocalTimeSafe tzc zi utc = fromMaybe (utcToLocalTime Local.utc utc) (zoneInfoToLocalTime tzc zi utc)
zoneInfoToUTCTimeSafe  tzc zi lcl  = fromMaybe (localTimeToUTC Local.utc lcl) (zoneInfoToUTCTime tzc zi lcl)
zoneInfoToTimeZoneSafe tzc zi utc  = fromMaybe (Local.utc) (zoneInfoToTimeZone tzc zi utc)

-- | Creates a TimeZoneConverter exploring the given dictionary.
-- Throws an Exception if no TimeZoneName found in the subdirectories
-- of the given path
-- Usage: createTimeZoneConverter "/usr/share/zoneinfo"
createTimeZoneConverter :: FilePath -> IO TimeZoneConverter
createTimeZoneConverter dir = do
  zoneInfos <- exploreTimeZoneNames dir

  when (null zoneInfos) . error $ "No TimeZoneName was found in the path: " ++ dir

  let zoneMap = Map.fromList zoneInfos

      localTime zi utc =
        fmap (\series -> utcToLocalTime' series utc)
             (Map.lookup zi zoneMap)

      utcTime zi lcl =
        fmap (\series -> localTimeToUTC' series lcl)
             (Map.lookup zi zoneMap)

      timeZone zi utc =
        fmap (\series -> timeZoneFromSeries series utc)
             (Map.lookup zi zoneMap)

  return $! TimeZoneConverter {
      zoneInfoToLocalTime = localTime
    , zoneInfoToUTCTime   = utcTime
    , zoneInfoToTimeZone  = timeZone
    , zoneInfos = Map.keys zoneMap
    }



-- | Exlpores the TimeZoneName subdirectories and load the TimeZoneSeries
-- from the disk
exploreTimeZoneNames :: FilePath -> IO [(TimeZoneName, TimeZoneSeries)]
exploreTimeZoneNames dir = exploreDirectory onFile combine dir
  where
    l = length dir + 1

    onFile path = catch
      (do series <- getTimeZoneSeriesFromOlsonFile path
          return [(TimeZoneName (drop l  path), series)])
      emptyList

    combine [] = return []
    combine xs = return $ foldl1 (++) xs

    emptyList :: SomeException -> IO [(TimeZoneName, TimeZoneSeries)]
    emptyList ex = do
      print ex
      return []

-- | Explores all the directory and runs a computation on each file and combines the result
exploreDirectory :: (FilePath -> IO a) -> ([a] -> IO a) -> FilePath -> IO a
exploreDirectory onFile combine base = do
  contents <- fmap (filter (`notElem` [".", ".."])) $ getDirectoryContents base
  xs <- forM contents $ \path -> do
    let base' = base </> path
    isDir <- doesDirectoryExist base'
    if isDir
       then exploreDirectory onFile combine base'
       else do isFile <- doesFileExist base'
               if isFile
                 then onFile base'
                 else error $ "exploreDirectory: It was not a file: " ++ show base'
  combine xs
