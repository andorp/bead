{-# LANGUAGE CPP #-}
module Bead.Persistence.SQL.Course where

import           Control.Applicative
import           Data.Maybe
import qualified Data.Text as Text

import           Database.Persist.Sqlite

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities

#ifdef TEST
import           Bead.Persistence.SQL.User
import           Test.Themis.Test (ioTest)
import           Test.Themis.Keyword.Encaps
#endif

-- * Course Persistence

-- Saves a Course into the database
saveCourse :: Domain.Course -> Persist Domain.CourseKey
saveCourse course = do
  key <- insert (fromDomainValue course)
  return $! toDomainKey key

-- Lists all the course keys saved in the database
courseKeys :: Persist [Domain.CourseKey]
courseKeys = do
  courses <- selectCourses
  return $! map (toDomainKey . entityKey) courses
  where
    selectCourses :: Persist [Entity Course]
    selectCourses = selectList [] []

-- Selects all the courses with satisfies the given property
filterCourses :: (Domain.CourseKey -> Domain.Course -> Bool) -> Persist [(Domain.CourseKey, Domain.Course)]
filterCourses pred = do
  courses <- selectCourses
  return $! filter (uncurry pred) $ map fromEntity courses
  where
    selectCourses :: Persist [Entity Course]
    selectCourses = selectList [] []

    fromEntity :: Entity Course -> (Domain.CourseKey, Domain.Course)
    fromEntity e = (toDomainKey $ entityKey e, toDomainValue $ entityVal e)

-- Load the course from the database
loadCourse :: Domain.CourseKey -> Persist Domain.Course
loadCourse courseKey = do
  mCourse <- get (fromDomainKey courseKey)
  return $! case mCourse of
    Nothing     -> persistError "loadCourse" $ "no course is found:" ++ show courseKey
    Just course -> toDomainValue course

-- Lists all the groups keys for the given course, the listed groups
-- are the groups under the given course
groupKeysOfCourse :: Domain.CourseKey -> Persist [Domain.GroupKey]
groupKeysOfCourse key = do
  let courseKey = fromDomainKey key
  groups <- selectList [GroupsOfCourseCourse ==. courseKey] []
  return $! map (toDomainKey . groupsOfCourseGroup . entityVal) groups

-- Checks if the user attends the given course
isUserInCourse :: Domain.Username -> Domain.CourseKey -> Persist Bool
isUserInCourse username courseKey = do
  let courseId = fromDomainKey courseKey
  withUser
    username
    (return False)
    (fmap isJust . getBy . UniqueUsersOfCoursePair courseId . entityKey)

-- Lists all the courses which the user attends
userCourses :: Domain.Username -> Persist [Domain.CourseKey]
userCourses username = withUser username (return []) $ \user ->
  map (toDomainKey . usersOfCourseCourse . entityVal)
    <$> selectList [UsersOfCourseUser ==. entityKey user] []

-- Set the given user as an administrator for the course
createCourseAdmin :: Domain.Username -> Domain.CourseKey -> Persist ()
createCourseAdmin username courseKey = withUser username (return ()) $ \userEnt -> void $ do
  insertUnique (AdminsOfCourse (toEntityKey courseKey) (entityKey userEnt))

-- Lists all the users which are administrators of the given course
courseAdmins :: Domain.CourseKey -> Persist [Domain.Username]
courseAdmins key = do
  let courseKey = fromDomainKey key
  userIds <- map (adminsOfCourseAdmin . entityVal) <$> selectList [AdminsOfCourseCourse ==. courseKey] []
  usernames userIds

-- Select all the existing usernames for the given user id list
usernames :: [UserId] -> Persist [Domain.Username]
usernames userIds = catMaybes <$> (mapM toUsername userIds)
  where
    toUsername e = do
      mUser <- get e
      return $! (Domain.Username . Text.unpack . userUsername <$> mUser)

#ifdef TEST

courseAdminTests = do
  let course = Domain.Course "name" "desc" Domain.TestScriptSimple
      user1name = Domain.Username "user1"
      user1  = Domain.User Domain.Student user1name                 (Domain.Email "email") "name" (Domain.TimeZoneName "UTC") (Domain.Language "hu")
      user2  = Domain.User Domain.Student (Domain.Username "user2") (Domain.Email "email") "name" (Domain.TimeZoneName "UTC") (Domain.Language "hu")

  ioTest "Create Course Admin for the course" $ runSql $ do
    dbStep initDB
    dbStep $ saveUser user1
    c <- dbStep $ saveCourse course
    acs <- dbStep $ administratedCourses user1name
    assertEquals [] (map fst acs) "There were courses that the user should not administrate."
    let u1 = Domain.u_username user1
    dbStep $ createCourseAdmin u1 c
    us <- dbStep $ courseAdmins c
    assertEquals [u1] us "Admins of course were different."
    acs <- dbStep $ administratedCourses user1name
    assertEquals [c] (map fst acs) "The administrated course list was wrong."

  ioTest "No Course Admin for the course" $ runSql $ do
    dbStep $ initDB
    c <- dbStep $ saveCourse course
    us <- dbStep $ courseAdmins c
    assertEquals [] us "Some admin was found for the course."

  ioTest "Same user is created as admin twice" $ runSql $ do
    dbStep $ initDB
    dbStep $ saveUser user1
    c <- dbStep $ saveCourse course
    let u1 = Domain.u_username user1
    dbStep $ createCourseAdmin u1 c
    dbStep $ createCourseAdmin u1 c
    us <- dbStep $ courseAdmins c
    assertEquals [u1] us "Admins of course were different."

  ioTest "Two different users administrates the same course" $ runSql $ do
    dbStep $ initDB
    dbStep $ saveUser user1
    dbStep $ saveUser user2
    c <- dbStep $ saveCourse course
    let u1 = Domain.u_username user1
        u2 = Domain.u_username user2
        users us = and [elem u1 us, elem u2 us]
    dbStep $ createCourseAdmin u1 c
    dbStep $ createCourseAdmin u2 c
    us <- dbStep $ courseAdmins c
    satisfies us users "Admins of course were different."

#endif

-- Lists all the users that are attends as a student on the given course
subscribedToCourse :: Domain.CourseKey -> Persist [Domain.Username]
subscribedToCourse key = do
  let courseKey = fromDomainKey key
  userIds <- map (usersOfCourseUser . entityVal) <$> selectList [UsersOfCourseCourse ==. courseKey] []
  usernames userIds

-- Lists all the users that are unsubscribed once from the given course
unsubscribedFromCourse :: Domain.CourseKey -> Persist [Domain.Username]
unsubscribedFromCourse key = do
  let courseKey = fromDomainKey key
  userIds <- map (unsubscribedUsersFromCourseUser . entityVal)
               <$> selectList [UnsubscribedUsersFromCourseCourse ==. courseKey] []
  usernames userIds

-- Lists all the test scripts that are connected with the course
testScriptsOfCourse :: Domain.CourseKey -> Persist [Domain.TestScriptKey]
testScriptsOfCourse key = do
  let courseKey = fromDomainKey key
  map (toDomainKey . testScriptsOfCourseTestScript . entityVal)
    <$> selectList [TestScriptsOfCourseCourse ==. courseKey] []
