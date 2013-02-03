module Bead.View.Cli.Login where

import Bead.Domain.Entities
import Bead.Domain.Types
import qualified Bead.Controller.Pages as P

import Bead.Controller.ServiceContext
import Bead.Controller.Logging
import qualified Bead.Controller.UserStories as Story
--import qualified Bead.Persistence.DirPersist as Persist
import qualified Bead.Persistence.NoSQLDir as Persist

import Data.IORef (IORef)
import qualified Data.IORef as Ref

import Data.Char (toLower)
import Data.List (isPrefixOf)

-- For mocking purpouses only
import qualified Bead.Persistence.Persist as Mock



login :: IO UserAction
login = do
  putStrLn "Username"
  username <- getLine
  putStrLn "Password"
  password <- getLine
  return $ Login (read username) password

logout :: IO ()
logout = putStrLn "You are logged out"

home :: UserState -> IO UserAction
home usr
  | isHomePageData (pageRenderData usr) = do
      putStrLn $ "Welcome " ++ show (user usr) ++ "!"
      putStrLn $ "You attempt the following courses:"
      putStrLn $ "You attempt in the following groups:"
      putStrLn $ "The following exams are waiting for you:"
      putStrLn $ "Please enter the code to navigate to the appropiate page: "
      putStrLn $ ">logout"
      putStrLn $ ">profile"
      putStrLn $ ">course key"
      putStrLn $ ">group key"
      putStrLn $ ">admin"
      s <- getLine
      return $ case map toLower s of
        "logout" -> Logout
        "profile" -> Profile
        "admin"   -> ChangePage P.Admin
        'c':'o':'u':'r':'s':'e':' ':s -> SelectCourse $ Encrypted s
        'g':'r':'o':'u':'p':' ':s -> SelectGroup $ Encrypted s
        _ -> undefined
  | otherwise = do
      putStrLn $ "Error: no course data was provided for homepage"
      return Logout

profile :: UserState -> IO UserAction
profile usr = do
  putStrLn $ "Username: " ++ show (user usr) ++ "!"
  putStrLn $ "Family name: " ++ (name usr)
  putStrLn $ ">change password newpassword newpassword"
  putStrLn $ ">home"
  s <- getLine
  return $ command s
  where
    command s
      | isPrefixOf "change" s = let [_,oldpwd,new,new'] = words s
                                in ChangePwd oldpwd new new'
      | otherwise             = ChangePage P.Home

admin :: UserState -> IO UserAction
admin usr = do
  putStrLn $ ">create-user username password password email role name"
  putStrLn $ ">create-course coursecode coursename"
  putStrLn $ ">create-group course groupname"
  putStrLn $ ">create-course-exam course date"
  putStrLn $ ">create-group-exam course group date"
  putStrLn $ ">home"
  s <- getLine
  return $ command s
  where
     command s
       | isPrefixOf "create-user" s =
           case words s of
             [_,uname,password,password',email,role,name] ->
               case password == password' of
                 False -> LogMessage "Passwords are not the same."
                 True  -> CreateUser 
                           User {
                               u_role = read role
                             , u_username = username uname
                             , u_email = Email email
                             , u_name  = name
                             }
                           password
             _ -> LogMessage $ "Invalid admin command: " ++ s

       | isPrefixOf "create-course" s = 
           case words s of
             [_,code,name] -> CreateCourse Course {
                                  courseCode = CourseCode code
                                , courseName = name
                                , courseDesc = "Course Description"
                                }
             _ -> LogMessage $ "Invalid admin command: " ++ s
       | isPrefixOf "create-group"  s = undefined
       | isPrefixOf "create-course-exam" s = undefined
       | isPrefixOf "create-group-exam"  s = undefined
       | isPrefixOf "home" s = ChangePage P.Home
       | otherwise           = ChangePage P.Admin

-- | The user can preform the following actions on the user interface
data UserAction
  -- Navigation
  = Logout
  | SelectCourse Encrypted
  | SelectGroup Encrypted
  | Login Username Password
  | ChangePage P.Page
  | LogMessage String

  -- Profiling
  | Profile
  | ChangePwd Password Password Password

  -- Group
  | CreateGroup String
  | DeleteGroup Encrypted

  -- Cource
  | CreateCourse Course
  | DeleteCourse Encrypted

  -- Exercise
  | CreateExercise String
  | DeleteExercise String

  -- Solution
  | SubmitSolution Encrypted String

  -- Administration
  | CreateUser User Password
  -- etc
  deriving (Eq)

data Session a = Session {
    sessionID   :: String
  , sessionData :: a
  } deriving (Eq, Show)

userActionStory :: UserState -> UserAction -> Story.UserStory ()
userActionStory state Logout             = Story.logout (user state)
userActionStory state Profile            = Story.changePage P.Profile
userActionStory state (ChangePage p)     = Story.changePage p
userActionStory state (ChangePwd o n n') = Story.changePassword o n n'
userActionStory state (CreateUser u p)   = Story.createUser u p
userActionStory state (LogMessage m)     = Story.logErrorMessage m
userActionStory state (CreateCourse c)   = Story.createCourse c >> return ()
userActionStory _     _                  = Story.noOperation
-- etc ...

singleSessionManager :: IO ()
singleSessionManager = do
  p <- Ref.newIORef (Session "single-session" testUserState)
  c <- Ref.newIORef (serviceContext Persist.noSqlDirPersist mockUserContainer mockLogger)

  let loop = do sessionState <- Ref.readIORef p
                servContext  <- Ref.readIORef c
                let userState = sessionData sessionState
                userStory <- case actualPage userState of

                  P.Login -> do (Login username password) <- login
                                return (Story.login username password)

                  P.Home -> home userState >>= return . (userActionStory userState)

                  P.Profile     -> profile userState >>= return . (userActionStory userState)

                  P.Course      -> undefined
                  P.Group       -> undefined
                  P.OpenExam    -> undefined
                  P.ClosedExam  -> undefined
                  P.Error       -> undefined
                  P.SubmitExam  -> undefined
                  P.Evaulation  -> undefined
                  P.Training    -> undefined
                  P.Admin       -> admin userState >>= return . (userActionStory userState)

                  page -> return $ Story.errorPage $ "Unknown page type: " ++ show page

                errorOrState <- Story.runUserStory servContext userState userStory
                case errorOrState of
                  Left err -> putStrLn (show err) >> loop
                  Right (_,state') -> do
                    putStrLn $ "New state."
                    Ref.writeIORef p (sessionState { sessionData = state' })
                    loop

  loop

testUserState = UserNotLoggedIn

{-
testUserState = UserState {
    user = username "and.or"
  , page = P.Login
  , name = "Andor Penzes"
  , role = Admin
  , availablePages = []
  , pageRenderData = NoPageData
  }
-}
