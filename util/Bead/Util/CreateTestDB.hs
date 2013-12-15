module Bead.Util.CreateTestDB where

{-
Creates a random database in the current directory and saves
all the created entries in json format
-}

import Control.Applicative
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Aeson
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Persistence.Persist
import qualified Bead.View.Snap.Registration as Reg

import qualified Test.Quick.EntityGen as Gen
import Test.Quick.Persistence

data UserInfo = UserInfo {
    userInfoUsername :: Username
  , userInfoPassword :: Password
  }

data CourseInfo = CourseInfo {
    courseInfoCourseKey  :: CourseKey
  , courseInfoCourseName :: String
  }

data GroupInfo = GroupInfo {
    groupInfoGroupKey  :: GroupKey
  , groupInfoCourseKey :: CourseKey
  , groupInfoGroupName :: String
  }

data AssignmentInfo = AssignmentInfo {
    asgInfoAsgKey  :: AssignmentKey
  , asgInfoAsgName :: String
  , asgInfoCourseOrGroupKey :: Either CourseKey GroupKey
  }

data SubmissionInfo = SubmissionInfo {
    sbmInfoSbmKey    :: SubmissionKey
  , sbmInfoSbmAsgKey :: AssignmentKey
  , sbmInfoUsername  :: Username
  }

data EvaluationInfo = EvaluationInfo {
    evInfoEvKey   :: EvaluationKey
  }

data CommentInfo = CommentInfo {
    cmtInfoCmtKey :: CommentKey
  , cmtInfoComment :: String
  }

passwords = return "password"

create r n = do
  list <- createListRef
  quick n $ do
    u <- setRole r <$> pick Gen.users
    p <- pick passwords
    run $ do
      Reg.createUser persist "users.json" u p
      insertListRef list ((u_username u),p)
  listInRef list

type SetMap k a = Map k (Set a)

insertSetMap :: (Ord k, Ord a) => k -> a -> SetMap k a -> SetMap k a
insertSetMap k a m = case Map.lookup k m of
  Nothing -> Map.insert k (Set.singleton a) m
  Just as -> Map.insert k (Set.insert a as) m

-- Select a student and a group and make subscribe the selected
-- student to the selected group
subscriptions n students groups = do
  ref <- newIORef (Map.empty)
  quick n $ do
    u <- pick $ elements students
    g <- pick $ elements groups
    c <- runPersistCmd $ courseOfGroup persist g
    runPersistCmd $ subscribe persist u c g
    run $ modifyIORef ref (insertSetMap u g)
  readIORef ref

-- Calculates a map for the user related course assignment keys, browsing
-- through the given subscriptionMap that contains the group set for
-- every user
courseUserAssignmentMap subscriptionMap = Map.foldWithKey userCourseAssignments empty subscriptionMap where
  empty = return Map.empty

  userCourseAssignments
    :: Username -> Set GroupKey -> IO (SetMap Username AssignmentKey)
                                -> IO (SetMap Username AssignmentKey)
  userCourseAssignments user groupSet newMap = Set.foldl (course user) newMap groupSet
    where
      course user map groupKey = do
        m <- map
        as <- runPersistIOCmd (courseOfGroup persist groupKey >>= courseAssignments persist)
        return $ foldl (\m' a' -> insertSetMap user a' m') m as

-- Calculates a map for the user related group assignment keys, browsing
-- through the given subscriptionMap that contains the group set for
-- every user
groupUserAssignmentMap subscriptionMap = Map.foldWithKey userGroupAssignments empty subscriptionMap where
  empty = return Map.empty

  userGroupAssignments user groupSet newMap = Set.foldl (group user) newMap groupSet
    where
      group user map groupKey = do
        m <- map
        as <- runPersistIOCmd (groupAssignments persist groupKey)
        return $ foldl (\m' a' -> insertSetMap user a' m') m as


-- Creates submissions selection a user
studentSubmissions n assignmentMap = do
  let users = elements $ Map.keys assignmentMap
      userAssignments u = elements . maybe [] (Set.toList) $ Map.lookup u assignmentMap
  ref <- newIORef Map.empty
  quick n $ do
    u <- pick users
    a <- pick $ userAssignments u
    sk <- saveAndLoadSubmissionFor u a
    run . modifyIORef ref $ insertSetMap (u,a) sk
  readIORef ref

data Config = Config {
    cfgNoOfAdmins :: Int
  , cfgNoOfCourseAdmins :: Int
  , cfgNoOfGroupAdmins :: Int
  , cfgNoOfStudents :: Int
  , cfgNoOfCourses :: Int
  , cfgNoOfCourseAdminRelation :: Int
  , cfgNoOfGroups :: Int
  , cfgNoOfGroupAdminRelation :: Int
  , cfgNoOfGroupAssignments :: Int
  , cfgNoOfCourseAssignments :: Int
  , cfgNoOfGroupSubscriptions :: Int
  , cfgNoOfCourseSubmissions :: Int
  , cfgNoOfGroupSubmissions :: Int
  , cfgNoOfEvaluations :: Int
  }

defaultConfig = Config {
    cfgNoOfAdmins = 2
  , cfgNoOfCourseAdmins = 10
  , cfgNoOfGroupAdmins = 20
  , cfgNoOfStudents = 50
  , cfgNoOfCourses = 10
  , cfgNoOfCourseAdminRelation = 15
  , cfgNoOfGroups = 30
  , cfgNoOfGroupAdminRelation = 25
  , cfgNoOfGroupAssignments = 60
  , cfgNoOfCourseAssignments = 40
  , cfgNoOfGroupSubscriptions = 100
  , cfgNoOfCourseSubmissions = 150
  , cfgNoOfGroupSubmissions = 150
  , cfgNoOfEvaluations = 400
  }

-- TODO: Saving information in JSON format as well
main = do
  let cfg = defaultConfig
  putStrLn "Clean persistence layer"
  reinitPersistence

  putStrLn "Creating admins"
  admins       <- create Admin (cfgNoOfAdmins cfg)

  putStrLn "Creating course admins"
  courseAdmins <- create CourseAdmin (cfgNoOfCourseAdmins cfg)

  putStrLn "Creating group admins"
  groupAdmins  <- create GroupAdmin (cfgNoOfGroupAdmins cfg)

  putStrLn "Creating students"
  students     <- create Student (cfgNoOfStudents cfg)

  let adminUsers = map fst admins
      courseAdminUsers = map fst courseAdmins
      groupAdminUsers = map fst groupAdmins
      studentUsers = map fst students

  putStrLn "Creating courses"
  cs <- courses (cfgNoOfCourses cfg)

  putStrLn "Assign course admins with courses"
  createCourseAdmins (cfgNoOfCourseAdminRelation cfg) courseAdminUsers cs

  putStrLn "Creating groups"
  gs <- groups (cfgNoOfGroups cfg) cs

  putStrLn "Assign group admins with groups"
  createGroupAdmins (cfgNoOfGroupAdminRelation cfg) groupAdminUsers gs

  putStrLn "Assign students with groups"
  subscriptionMap <- subscriptions (cfgNoOfGroupSubscriptions cfg) studentUsers gs

  putStrLn "Creating course assignments"
  groupAssignments <- groupAssignmentGen (cfgNoOfGroupAssignments cfg) gs

  putStrLn "Creating group assignments"
  courseAssignments <- courseAssignmentGen (cfgNoOfCourseAssignments cfg) cs

  let assignments = groupAssignments ++ courseAssignments

  putStrLn "Calculating user course assignments"
  userCourseAssignments <- courseUserAssignmentMap subscriptionMap

  putStrLn "Calculating user group assignments"
  userGroupAssignments <- groupUserAssignmentMap subscriptionMap

  putStrLn "Submitting course submissions"
  csk <- studentSubmissions (cfgNoOfCourseSubmissions cfg) userCourseAssignments

  putStrLn "Submitting group submissions"
  gsk <- studentSubmissions (cfgNoOfGroupSubmissions cfg) userGroupAssignments

  let sk = Map.union csk gsk

  putStrLn "Creating evaulations"
  ek <- evaluations (cfgNoOfEvaluations cfg) (concatMap (Set.toList . snd) $ Map.toList sk)

  return ()

setRole :: Role -> User -> User
setRole r u = u { u_role = r }
