{-# LANGUAGE CPP #-}
module Bead.Persistence.SQL.Group where

import           Control.Applicative
import           Control.Monad (when)
import           Data.Maybe
import qualified Data.Text as Text

import           Database.Persist.Sql

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import           Bead.Persistence.SQL.Entities
import           Bead.Persistence.SQL.Class

#ifdef TEST
import qualified Data.Set as Set

import           Bead.Persistence.SQL.Course
import           Bead.Persistence.SQL.User

import           Bead.Persistence.SQL.TestData

import           Test.Tasty.TestSet (ioTest, equals)
#endif

-- * Group Persistence

-- Save the group under the given course
saveGroup :: Domain.CourseKey -> Domain.Group -> Persist Domain.GroupKey
saveGroup key group = do
  let courseKey = fromDomainKey key
  groupKey <- insert (fromDomainValue group)
  insertUnique (GroupsOfCourse courseKey groupKey)
  return $! toDomainKey groupKey

-- Load the group from the database
loadGroup :: Domain.GroupKey -> Persist Domain.Group
loadGroup groupKey = do
  mGroup <- get (fromDomainKey groupKey)
  return $! case mGroup of
    Nothing    -> persistError "loadGroup" $ "no group is found:" ++ show groupKey
    Just group -> toDomainValue group

-- Returns the course of the given group
courseOfGroup :: Domain.GroupKey -> Persist Domain.CourseKey
courseOfGroup key = do
  mCourseGroup <- getBy . UniqueGroupCourseGroup $ fromDomainKey key
  return $! case mCourseGroup of
    Nothing  -> persistError "courseOfGroup" $ "no group is found:" ++ show key
    Just ent -> toDomainKey . groupsOfCourseCourse $ entityVal ent

-- Lists all the groups from the database that satisfies the given predicate
filterGroups :: (Domain.GroupKey -> Domain.Group -> Bool) -> Persist [(Domain.GroupKey, Domain.Group)]
filterGroups pred = (filter (uncurry pred) . map toDomainGroupPair) <$> selectList [] []
  where
    toDomainGroupPair e = (toDomainKey $ entityKey e, toDomainValue $ entityVal e)

-- Returns True if the user is registered in the group, otherwise False
isUserInGroup :: Domain.Username -> Domain.GroupKey -> Persist Bool
isUserInGroup username groupKey = withUser username (return False) $ \userEnt ->
  isJust <$> getBy (UniqueUsersOfGroupPair (toEntityKey groupKey) (entityKey userEnt))

-- Lists all the groups that the user is attended in
userGroups :: Domain.Username -> Persist [Domain.GroupKey]
userGroups username = withUser username (return []) $ \userEnt ->
  map (toDomainKey . usersOfGroupGroup . entityVal)
    <$> selectList [UsersOfGroupUser ==. entityKey userEnt] []

-- Subscribe the user for the given course and group
subscribe :: Domain.Username -> Domain.CourseKey -> Domain.GroupKey -> Persist ()
subscribe username courseKey groupKey = withUser username (return ()) $ \userEnt -> void $ do
  let userKey = entityKey userEnt
  insertUnique (UsersOfGroup  (toEntityKey groupKey)  userKey)
  insertUnique (UsersOfCourse (toEntityKey courseKey) userKey)

-- Unsubscribe the user from the given course and group,
-- if the user is not subscribed nothing happens
unsubscribe :: Domain.Username -> Domain.CourseKey -> Domain.GroupKey -> Persist ()
unsubscribe username courseDomKey groupDomKey = withUser username (return ()) $ \userEnt -> void $ do
  let groupKey = toEntityKey groupDomKey
      courseKey = toEntityKey courseDomKey
      userKey = entityKey userEnt
      groupFilter = [UsersOfGroupUser ==. userKey, UsersOfGroupGroup ==. groupKey]
      courseFilter = [UsersOfCourseUser ==. userKey, UsersOfCourseCourse ==. courseKey]
  groups <- selectList groupFilter []
  courses <- selectList courseFilter []
  when (or [not $ null groups, not $ null courses]) . void $ do
    deleteWhere groupFilter
    deleteWhere courseFilter
    insertUnique (UnsubscribedUsersFromGroup  groupKey  userKey)
    insertUnique (UnsubscribedUsersFromCourse courseKey userKey)

#ifdef TEST

groupTests = do
  ioTest "Create and load group" $ runSql $ do
    initDB
    c <- saveCourse course
    g <- saveGroup  c group
    group' <- loadGroup g
    equals group group' "Group was saved and load incorrectly"

  ioTest "Course key of group was saved correctly" $ runSql $ do
    initDB
    c <- saveCourse course
    g <- saveGroup  c group
    c' <- courseOfGroup g
    equals c c' "Course key was not loaded correctly"

  ioTest "Check group subscription" $ runSql $ do
    initDB
    saveUser user1
    c <- saveCourse course
    g <- saveGroup  c group

    ingr <- isUserInGroup user1name g
    equals False ingr "User was in the group"

    subscribe user1name c g
    ingr <- isUserInGroup user1name g
    equals True ingr "User was not in the subscribed group"

  ioTest "Check creating group admins" $ runSql $ do
    initDB
    saveUser user1
    saveUser user2
    c <- saveCourse course
    g <- saveGroup c group
    ags <- administratedGroups user1name
    equals [] (map fst ags) "There was group administrated with the user"
    admins <- groupAdmins g
    equals [] admins "There were group admins, without creation"
    createGroupAdmin user1name g
    admins <- groupAdmins g
    equals [user1name] admins "The first admin was not assigned to the group"
    ags <- administratedGroups user1name
    equals [g] (map fst ags) "There was no group administrated with the user"
    createGroupAdmin user2name g
    admins <- groupAdmins g
    equals
      (Set.fromList [user1name, user2name])
      (Set.fromList admins)
      "The admins were different to the group"

  ioTest "Check the user subscription and unsubscription from the course" $ runSql $ do
    initDB
    saveUser user1
    saveUser user2
    c <- saveCourse course
    g <- saveGroup c group
    subscribe user1name c g
    unsubscribe user1name c g
    us <- unsubscribedFromGroup g
    equals [user1name] us "User was not unsubscribed from the group"

#endif

-- Calculates the domain username from the user entity
usernameFromEntity = Domain.Username . Text.unpack . userUsername

-- Lists all the group admins for the given course
groupAdmins :: Domain.GroupKey -> Persist [Domain.Username]
groupAdmins groupKey = do
  groupAdmins <- selectList [AdminsOfGroupGroup ==. (toEntityKey groupKey)] []
  (map usernameFromEntity . catMaybes) <$> mapM (get . adminsOfGroupAdmin . entityVal) groupAdmins

-- Set the given user for the given group
createGroupAdmin :: Domain.Username -> Domain.GroupKey -> Persist ()
createGroupAdmin username groupKey = withUser username (return ()) $ \userEnt -> void $ do
  insertUnique (AdminsOfGroup (toEntityKey groupKey) (entityKey userEnt))

-- Lists all the users that are subscribed to the given group
subscribedToGroup :: Domain.GroupKey -> Persist [Domain.Username]
subscribedToGroup groupKey = do
  userIds <- selectList [UsersOfGroupGroup ==. (toEntityKey groupKey)] []
  (map usernameFromEntity . catMaybes) <$> mapM (get . usersOfGroupUser . entityVal) userIds

-- Lists all the users that are unsubscribed from the given group at least once
unsubscribedFromGroup :: Domain.GroupKey -> Persist [Domain.Username]
unsubscribedFromGroup groupKey = do
  userIds <- selectList [UnsubscribedUsersFromGroupGroup ==. (toEntityKey groupKey)] []
  (map usernameFromEntity . catMaybes) <$> mapM (get . unsubscribedUsersFromGroupUser . entityVal) userIds
