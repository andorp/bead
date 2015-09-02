module Bead.Daemon.LDAP.Query (
    QuerySettings(..)
  , QueryResult(..)
  , queryResult
  , query
  ) where

import           Control.Applicative
import           Control.Arrow ((&&&), (***))
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.ByteString.Base64
import qualified Data.ByteString.Char8 as BS
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.String
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process
import           System.Timeout
import           Text.Printf

data QueryResult
  = QueryOK [(String,String)]
  | QueryInvalid
  | QueryError String
  deriving (Eq,Show)

queryResult
  queryOk
  queryInvalid
  queryError
  q = case q of
    QueryOK attrs  -> queryOk attrs
    QueryInvalid   -> queryInvalid
    QueryError msg -> queryError msg

data QuerySettings = QuerySettings
  { queryTimeout :: Int
  , queryCommand :: String
  }

exitCode :: (Int -> a) -> a -> ExitCode -> a
exitCode _ x ExitSuccess     = x
exitCode f _ (ExitFailure y) = f y

exitCode' f x = exitCode (const f) x

raise :: String -> Query a
raise = left

type Query a = EitherT String (ReaderT QuerySettings IO) a

ldapsearch :: String -> [String] -> Query [(String,String)]
ldapsearch uid attrs = do
  (t,cmd:args) <- asks $ queryTimeout &&& (words . queryCommand)
  (ec,out,err) <- liftIO (timeout (t * 1000000) $
    readProcessWithExitCode
      cmd (args ++ ["uid=" ++ uid] ++ attrs)
      "")
    >>= maybe (raise "timed out") return
  let out1 = DT.replace (fromString "\n ") (fromString "") $ fromString out
  let out2 = filter (not . DT.null) $ DT.lines out1
  let r    = map ((DT.unpack *** translate) . DT.break (== ':')) out2
  exitCode' (raise err) (return r) ec
 where
   textToBS :: DT.Text -> BS.ByteString
   textToBS = BS.pack . DT.unpack

   translate cs
     | colon `DT.isPrefixOf` cs      = DT.unpack cs1
     | colonColon `DT.isPrefixOf` cs =
       either (const $ DT.unpack cs2) (DT.unpack . DTE.decodeUtf8) (decode $ textToBS cs2)
     | otherwise = DT.unpack $ cs
     where
       colon      = fromString ": "
       colonColon = fromString ":: "
       Just cs1   = DT.stripPrefix colon cs
       Just cs2   = DT.stripPrefix colonColon cs

query :: QuerySettings -> String -> [String] -> IO QueryResult
query cfg usr attrs =
 fmap (either QueryError id) $ flip runReaderT cfg . runEitherT $ do
   QueryOK <$> ldapsearch usr attrs
