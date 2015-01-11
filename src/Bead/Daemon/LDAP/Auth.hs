module Bead.Daemon.LDAP.Auth (
    AuthSettings(..)
  , AuthResult(..)
  , authResult
  , Source(..)
  , authenticate
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

data AuthResult
  = AuthOK [(String,String)]
  | AuthInvalid
  | AuthError Source String
  deriving (Eq,Show)

authResult
  authOk
  authInvalid
  authError
  a = case a of
    AuthOK attrs -> authOk attrs
    AuthInvalid -> authInvalid
    AuthError source msg -> authError source msg

data AuthSettings = AuthSettings
  { authTimeOut :: Int
  , authCache   :: FilePath
  }

data Source = Kinit | Ldapsearch
  deriving Eq

instance Show Source where
  show Kinit      = "kinit(1)"
  show Ldapsearch = "ldapsearch(1)"

exitCode :: (Int -> a) -> a -> ExitCode -> a
exitCode _ x ExitSuccess     = x
exitCode f _ (ExitFailure y) = f y

exitCode' f x = exitCode (const f) x

raise :: Source -> String -> Auth a
raise = curry left

type Auth a = EitherT (Source,String) (ReaderT AuthSettings IO) a

kinit :: String -> String -> Auth Bool
kinit username password = do
 (t,cc) <- asks (authTimeOut &&& authCache)
 (ec,_out,_err) <- liftIO (timeout (t * 1000000) $
   readProcessWithExitCode
     "kinit" [ "--lifetime=5m", "--password-file=STDIN"
             , "--cache=" ++ printf cc username, username ]
     password)
   >>= maybe (raise Kinit "timed out") return
 return $ exitCode' False True ec

kdestroy :: String -> Auth ()
kdestroy usr = do
  cc <- asks authCache
  liftIO $ callCommand ("kdestroy --cache=" ++ printf cc usr)

ldapsearch :: String -> [String] -> Auth [(String,String)]
ldapsearch uid attrs = do
  (t,cc) <- asks (authTimeOut &&& authCache)
  (ec,out,err) <- liftIO (timeout (t * 1000000) $
    readProcessWithExitCode
      "env" (["KRB5CCNAME=" ++ printf cc uid, "ldapsearch",
        "-Q", "-LLL", "uid=" ++ uid] ++ attrs)
      "")
    >>= maybe (raise Ldapsearch "timed out") return
  let out1 = DT.replace (fromString "\n ") (fromString "") $ fromString out
  let out2 = filter (not . DT.null) $ DT.lines out1
  let r    = map ((DT.unpack *** translate) . DT.break (== ':')) out2
  exitCode' (raise Ldapsearch err) (return r) ec
 where
   translate cs
     | colon `DT.isPrefixOf` cs      = DT.unpack cs1
     | colonColon `DT.isPrefixOf` cs =
       either (const $ DT.unpack cs2) BS.unpack (decode $ DTE.encodeUtf8 cs2)
     | otherwise = DT.unpack $ cs
     where
       colon      = fromString ": "
       colonColon = fromString ":: "
       Just cs1   = DT.stripPrefix colon cs
       Just cs2   = DT.stripPrefix colonColon cs

authenticate :: AuthSettings -> String -> String -> [String] -> IO AuthResult
authenticate cfg usr pwd attrs =
 fmap (either (uncurry AuthError) id) $ flip runReaderT cfg . runEitherT $ do
   ok <- kinit usr pwd
   if ok
     then do
       as <- ldapsearch usr attrs
       kdestroy usr
       return $ AuthOK as
     else return AuthInvalid
