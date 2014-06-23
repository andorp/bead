module Bead.Persistence.SQL.User where

import           Control.Applicative
import           Data.Maybe
import qualified Data.Text as Text

import           Database.Persist.Sqlite

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities
import qualified Bead.Persistence.SQL.FileSystem as FS
import           Bead.Persistence.SQL.JSON

-- * User persistence

-- Save the current user, if the timezone and the role is stored in the database, otherwise
-- do nothing
saveUser :: Domain.User -> Persist ()
saveUser = void . insert . fromDomainValue

-- Calculates the personal information about the user
personalInfo :: Domain.Username -> Persist Domain.PersonalInfo
personalInfo username = do
  user <- loadUser username
  return $! Domain.withUser user $ \role _username _email name timezone _language ->
    Domain.PersonalInfo (role, name, timezone)

-- Select users who satiesfies the given predicate
filterUsers :: (Domain.User -> Bool) -> Persist [Domain.User]
filterUsers pred = do
  users <- selectList [] []
  return $! filter pred $ map (toDomainValue . entityVal) users

-- Loads the user information for the given username, supposing that the user
-- exists in the database
loadUser :: Domain.Username -> Persist Domain.User
loadUser = Domain.usernameCata $ \username -> do
  mUserEnt <- getBy . UniqueUsername $ Text.pack username
  case mUserEnt of
    Nothing -> persistError "loadUser" $ "Username is not found: " ++ username
    Just (Entity _entityKey userEnt) -> return $! toDomainValue userEnt

-- Updates the user information
updateUser :: Domain.User -> Persist ()
updateUser user = do
  userId <- entityKey <$> (getByUsername $ Domain.u_username user)
  update userId $ Domain.withUser user $ \role username email name timezone language ->
    [ UserRole     =. (encodeRole role)
    , UserUsername =. (Domain.usernameCata Text.pack username)
    , UserEmail    =. (Domain.emailCata Text.pack email)
    , UserName     =. (Text.pack name)
    , UserTimeZone =. (encodeTimeZone timezone)
    , UserLanguage =. (Domain.languageCata Text.pack language)
    ]

-- Checks if the user is already in the database
doesUserExist :: Domain.Username -> Persist Bool
doesUserExist = Domain.usernameCata $ \username -> do
  mUserEnt <- getBy . UniqueUsername $ Text.pack username
  return $! maybe False (const True) mUserEnt

-- Creates a description from the given username
userDescription :: Domain.Username -> Persist Domain.UserDesc
userDescription username = do
  user <- loadUser username
  return $! Domain.withUser user $ \_role username _email name _timezone _language ->
    Domain.UserDesc username name

-- Lists all the submission keys for the submissions that submitted by the user
-- for the given assignment
userSubmissions :: Domain.Username -> Domain.AssignmentKey -> Persist [Domain.SubmissionKey]
userSubmissions username key =
  withUser
    username
    (persistError "userSubmissions" $ "user is not found: " ++ show username)
    (\userEnt ->
       map (toDomainKey . userSubmissionOfAssignmentSubmission . entityVal) <$>
         selectList
           [ UserSubmissionOfAssignmentUser       ==. entityKey userEnt
           , UserSubmissionOfAssignmentAssignment ==. toEntityKey key
           ] [])

-- Lists all the courses that are administrated by the user
administratedCourses :: Domain.Username -> Persist [(Domain.CourseKey, Domain.Course)]
administratedCourses username =
  withUser
    username
    (persistError "administratedCourse" $ "user is not found: " ++ show username)
    (\userEnt -> do
       cks <- map (adminsOfCourseCourse . entityVal) <$>
                selectList [AdminsOfCourseAdmin ==. entityKey userEnt] []
       catMaybes <$> mapM getWithKey cks)
  where
    getWithKey k = do
      mVal <- get k
      return $ fmap (\x -> (toDomainKey k,toDomainValue x)) mVal

-- Lists all the groups that are administrated by the user
administratedGroups :: Domain.Username -> Persist [(Domain.GroupKey, Domain.Group)]
administratedGroups username =
  withUser
    username
    (persistError "administratedGroup" $ "user is not found: " ++ show username)
    (\userEnt -> do
       gks <- map (adminsOfGroupGroup . entityVal) <$>
                selectList [AdminsOfGroupAdmin ==. entityKey userEnt] []
       catMaybes <$> mapM getWithKey gks)
  where
    getWithKey k = do
      mVal <- get k
      return $ fmap (\x -> (toDomainKey k,toDomainValue x)) mVal

-- * Users file upload

-- Copies the given file with the given filename to the users data directory
copyFile :: Domain.Username -> FilePath -> Domain.UsersFile -> Persist ()
copyFile = FS.copyUsersFile

-- List all the user's files
listFiles :: Domain.Username -> Persist [(Domain.UsersFile, Domain.FileInfo)]
listFiles = FS.listFiles

-- Get the current path for the user's file
getFile :: Domain.Username -> Domain.UsersFile -> Persist FilePath
getFile = FS.getFile
