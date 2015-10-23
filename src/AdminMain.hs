{-# LANGUAGE CPP #-}
module AdminMain where

import           Control.Monad
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Reader
import           Data.Char (toUpper)
import qualified Data.Char as Char
import qualified Data.Map as Map
import           Data.Maybe

import           System.Console.GetOpt
import           System.Directory
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitFailure)
import           System.IO (hFlush, hSetEcho, stdout, stdin)
import           System.IO.Temp (createTempDirectory)
import           Text.Regex.TDFA

import           Bead.Config hiding (CreateAdmin)
#ifdef SSO
import           Bead.Daemon.LDAP.Query
#endif
import           Bead.Domain.Entities
import           Bead.Domain.TimeZone (utcZoneInfo)
import qualified Bead.Persistence.Persist as Persist
import qualified Bead.Persistence.Initialization as Persist
import           Bead.View.BeadContextInit
import qualified Bead.View.Registration as Registration
import           Bead.View.Validators hiding (toLower)

data Command
  = CreateAdmin
  | CreateStudent
  | ChangePassword String
#ifdef SSO
  | ImportUser String
#endif
  deriving (Eq, Show)

command
  createAdmin
  createStudent
  changePassword
#ifdef SSO
  importUser
#endif
  c = case c of
    CreateAdmin    -> createAdmin
    CreateStudent  -> createStudent
    ChangePassword username -> changePassword username
#ifdef SSO
    ImportUser username -> importUser username
#endif

type CLI a = ReaderT (Config, Persist.Interpreter) IO a

runCLI cfg = flip runReaderT cfg

getConfig :: CLI Config
getConfig  = asks fst

getPersist :: CLI Persist.Interpreter
getPersist = asks snd

main = do
  config <- readConfiguration beadConfigFileName
  args <- getArgs
  name <- getProgName
  persist <- createPersist $ Persist.configToPersistConfig config
  runCLI (config, persist) $ runCommands name args

options :: [OptDescr Command]
options = [
    Option ['a'] ["admin"]   (NoArg CreateAdmin)   "Create admin user."
  , Option ['s'] ["student"] (NoArg CreateStudent) "Create student user."
  , Option ['p'] ["password"] (ReqArg (ChangePassword . convertUsername) "USERNAME") "Change password for a user"
#ifdef SSO
  , Option ['i'] ["import"] (ReqArg (ImportUser . convertUsername) "USERNAME") "Import user from LDAP"
#endif
  ]

-- Parse the arguments and run the commands for the given arguments
runCommands :: String -> [String] -> CLI ()
runCommands progName args = case getOpt RequireOrder options args of
  ([], _, _)     -> usage
  (cmds, [], []) -> mapM_ runCommand cmds
  _ -> usage
  where
    usage = lift . putStrLn $ usageInfo headLine options
    headLine = unlines [ (concat ["Usage: ", progName, " (option)+ "])
                       , (concat ["Eg: ", progName, " -a -s -p user1 -p user2"])
                       ]

runCommand :: Command -> CLI ()
runCommand cmd = do
  persist <- getPersist
  cfg <- getConfig

  let createAdmin = do
        lift $ putStrLn "Creating Admin ..."
        userRegInfo <- readUserRegInfo
        lift $ Registration.createAdminUser persist usersJson userRegInfo

      createStudent = do
        lift $ putStrLn "Creating Student ..."
        userRegInfo <- readUserRegInfo
        lift $ Registration.createStudentUser persist usersJson userRegInfo

      changePassword username = do
        putStrLn $ concat ["Change password for ", username, " ..."]
        exists <- Persist.runPersist persist $ Persist.doesUserExist (Username username)
        case exists of
          (Left err) -> do
             putStrLn $ "Persistent error has happened: " ++ show err

          (Right False) -> do
             putStrLn $ "User does not exist:" ++ username

          (Right True) -> do
             pwd  <- readPassword "Password: "
             pwd' <- readPassword "Password Again: "
             case pwd == pwd' of
               False -> do
                 putStrLn "Passwords do not match!"
                 changePassword username
               True -> do
                 Registration.changeUserPassword usersJson (Username username) pwd

#ifdef SSO
      importUser username = do
        putStrLn $ concat ["Importing ", username, " from LDAP..."]
        let loginCfg = loginConfig cfg
        let settings = QuerySettings
                         { queryTimeout = sSOTimeout loginCfg
                         , queryCommand = sSOQueryCommand loginCfg
                         }
        let attributes = map ($ loginCfg) [sSOUserIdKey, sSOUserNameKey, sSOUserEmailKey]
        query settings username attributes >>= queryResult
          (\attrs -> do
             let attrMap = Map.fromList attrs
             case (map (flip Map.lookup attrMap) attributes) of
               [Just uid, Just fullName, Just email] -> do
                 let regInfo = UserRegInfo (username, uid, "password", email, fullName,
                                 TimeZoneName $ defaultRegistrationTimezone cfg)
                 Registration.createStudentUser persist usersJson regInfo
                 putStrLn "DONE"
               _ -> putStrLn "ERROR: Failed to map LDAP attributes to user details.")
          (putStrLn "ERROR: LDAP query could not run as it was invalid.")
          (\msg -> putStrLn $ concat ["ERROR: ", msg])
#endif

#ifdef SSO
  command createAdmin createStudent (lift . changePassword) (lift . importUser) cmd
#else
  command createAdmin createStudent (lift . changePassword) cmd
#endif

-- Create the persistent context for user creation
createPersist :: Persist.Config -> IO Persist.Interpreter
createPersist cfg = do
  init <- Persist.createPersistInit cfg
  isPersistSetUp <- Persist.isSetUp init
  case isPersistSetUp of
    True -> return ()
    False -> Persist.initPersist init
  Persist.createPersistInterpreter cfg

-- Read user information from stdin, validates the username, the passwords and email fields
readUserRegInfo :: CLI UserRegInfo
readUserRegInfo = do
  persist <- getPersist
  config <- getConfig
  join $ lift $ do
    putStrLn "Creating admin user, all characters are converted to lower case."
    putStrLn "Username must be in the defined format, which is given in the config."
    usr <- readUsername config
    exists <- Persist.runPersist persist $ Persist.doesUserExist (Username usr)
    case exists of
      (Left err) -> return $ do
        lift . putStrLn $ "Persistent error has happened: " ++ show err
        readUserRegInfo

      (Right True) -> return $ do
        lift $ putStrLn "User already exists!"
        readUserRegInfo

      (Right False) -> do
        email <- readEmail
        fullName <- readFullname
        uid      <- readUid
        pwd      <- readPassword "Password: "
        pwdAgain <- readPassword "Password Again: "
        hSetEcho stdin True
        case pwd == pwdAgain of
          -- All the validators are passed, the registration can be done
          True  -> return $ do
            cfg <- getConfig
            return $ UserRegInfo (usr, uid, pwd, email, fullName,
              TimeZoneName $ defaultRegistrationTimezone cfg)
          False -> return $ do
            lift $ putStrLn "Passwords do not match!"
            readUserRegInfo
    where
      putStrFlush msg = putStr msg >> hFlush stdout

      readFullname = do
        putStrFlush "Full name: "
        getLine

      readEmail = do
        putStrFlush "Email address: "
        email <- getLine
        validate
          isEmailAddress
          email
          (return email) -- Valid email
          (\msg -> do putStrLn msg
                      readEmail)

      readUid = do
        putStrFlush "User ID: "
        getLine

      readUsername cfg = do
        putStrFlush "User: "
        usr <- fmap convertUsername getLine
        validate
          isUsername
          usr
          -- Valid username
          (if (isValidUsername cfg usr)
             then return usr
             else do putStrLn "Username does not match the given regexp!"
                     readUsername cfg)
          -- Invalid username
          (\msg -> do putStrLn msg
                      readUsername cfg)
        where
          isValidUsername cfg usr =
#ifdef SSO
            and [length usr > 0, all Char.isAlphaNum usr]
#else
            usr =~ (usernameRegExp $ loginConfig cfg)
#endif

readPassword msg = do
  putStrFlush msg
  hSetEcho stdin False
  pwd <- getLine
  putStrLn ""
  hSetEcho stdin True
  validate
    isPassword
    pwd
    (return pwd)
    (\msg' -> do putStrLn msg'
                 readPassword msg)
  where
    putStrFlush msg = putStr msg >> hFlush stdout

convertUsername = map toUpper
