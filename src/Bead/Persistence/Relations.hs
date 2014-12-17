{-# LANGUAGE CPP #-}
module Bead.Persistence.Relations (
    userAssignmentKeys
  , userAssignmentKeyList
  , submissionDesc
  , submissionListDesc
  , submissionDetailsDesc
  , groupDescription
  , isAdminedSubmission
  , canUserCommentOn
  , submissionTables
  , courseSubmissionTableInfo
  , userSubmissionDesc
  , userLastSubmissionInfo
  , courseOrGroupOfAssignment
  , courseNameAndAdmins
  , administratedGroupsWithCourseName
  , groupsOfUsersCourse
  , removeOpenedSubmission
  , deleteUserFromCourse -- Deletes a user from a course, searching the group id for the unsubscription
  , isThereASubmissionForGroup -- Checks if the user submitted any solutions for the group
  , isThereASubmissionForCourse -- Checks if the user submitted any solutions for the course
  , testScriptInfo -- Calculates the test script information for the given test key
  , openedSubmissionInfo -- Calculates the opened submissions for the user from the administrated groups and courses
  , submissionLimitOfAssignment
#ifdef TEST
  , persistRelationsTests
#endif
  ) where

{-
This module contains higher level functionality for querying information
useing the primitves defined in the Persist module. Mainly Relations module
related information is computed.
-}

import           Control.Applicative
import           Control.Arrow
import           Control.Monad (foldM, forM, when)
import           Control.Monad.IO.Class
import           Data.Function (on)
import           Data.List ((\\), nub, sortBy, intersect, find)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime, getCurrentTime)

import           Bead.Domain.Entities
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.Domain.Relationships
import           Bead.Domain.Shared.Evaluation
import           Bead.Persistence.Persist
import           Bead.View.Snap.Translation

#ifdef TEST
import           Bead.Persistence.Initialization

import qualified Test.Themis.Keyword.Encaps as Keyword (step, key)
import           Test.Themis.Keyword.Encaps hiding (step, key)
import           Test.Themis.Test
import           Test.Themis.Test.Asserts hiding (assertEquals)
#endif

-- * Combined Persistence Tasks

-- Computes the Group key list, which should contain one element,
-- for a course key and a user, which the user attends in.
groupsOfUsersCourse :: Username -> CourseKey -> Persist [GroupKey]
groupsOfUsersCourse u ck = do
  ugs <- nub <$> userGroups u
  cgs <- nub <$> groupKeysOfCourse ck
  return $ intersect ugs cgs

-- Produces a Just Assignment list, if the user is registered for some courses,
-- otherwise Nothing.
userAssignmentKeys :: Username -> Persist (Map CourseKey (Set AssignmentKey))
userAssignmentKeys u = do
  gs <- nub <$> userGroups u
  cs <- nub <$> userCourses u
  case (cs,gs) of
    ([],[]) -> return Map.empty
    _       -> do
      gas <- foldM groupAssignment Map.empty gs
      as  <- foldM courseAssignment gas cs
      return $! as
  where
    groupAssignment m gk = do
      ck <- courseOfGroup gk
      (insert ck) <$> (Set.fromList <$> groupAssignments gk) <*> (pure m)

    courseAssignment m ck =
      (insert ck) <$> (Set.fromList <$> courseAssignments ck) <*> (pure m)

    insert k v m =
      maybe (Map.insert k v m) (flip (Map.insert k) m . (Set.union v)) $ Map.lookup k m

#ifdef TEST

userAssignmentKeysTest = do
  let course  = Course "name" "desc" TestScriptSimple
      group  = Group "name" "desc"
      asg     = Assignment "name" "desc" Assignment.emptyAspects time time binaryConfig
      time    = read "2014-06-09 12:55:27.959203 UTC"
      user1name = Username "user1"
      user1  = User Student user1name (Email "email") "name" (TimeZoneName "Europe/Budapest") (Language "hu")

  ioTest "User assignment keys with group and course assignments" $ do
    init <- createPersistInit defaultConfig
    interp <- createPersistInterpreter defaultConfig
    initPersist init
    result <- runPersist interp . runKeyword encapsContext $ do
      dbStep $ saveUser user1
      c <- dbStep $ saveCourse course
      g <- dbStep $ saveGroup c group
      a1 <- dbStep $ saveCourseAssignment c asg
      a2 <- dbStep $ saveCourseAssignment c asg
      a3 <- dbStep $ saveGroupAssignment g asg
      a4 <- dbStep $ saveGroupAssignment g asg
      keys <- dbStep $ userAssignmentKeys user1name
      assertEquals Map.empty keys "The unsubscribed user has some assignment keys"
      dbStep $ subscribe user1name c g
      keyMap <- dbStep $ userAssignmentKeys user1name
      assertEquals
        (Map.fromList [ (c,Set.fromList [a1,a2,a3,a4])])
        keyMap
        "The assignment of the users was different than the sum of the course and group assignment"
      return ()
    tearDown init
    return result
  return ()

#endif

-- Produces the assignment key list for the user, it the user
-- is not registered in any course the result is the empty list
userAssignmentKeyList :: Username -> Persist [AssignmentKey]
userAssignmentKeyList u = (nub . concat . map (Set.toList . snd) . Map.toList) <$> (userAssignmentKeys u)

courseOrGroupOfAssignment :: AssignmentKey -> Persist (Either CourseKey GroupKey)
courseOrGroupOfAssignment ak = do
  mGk <- groupOfAssignment ak
  case mGk of
    Just gk -> return . Right $ gk
    Nothing -> do
      mCk <- courseOfAssignment ak
      case mCk of
        Just ck -> return . Left $ ck
        Nothing -> error $ "Impossible: No course or groupkey was found for the assignment:" ++ show ak

administratedGroupsWithCourseName :: Username -> Persist [(GroupKey, Group, String)]
administratedGroupsWithCourseName u = do
  gs <- administratedGroups u
  forM gs $ \(gk,g) -> do
    fn <- fullGroupName gk
    return (gk,g,fn)

-- Produces a full name for a group including the name of the course.
fullGroupName :: GroupKey -> Persist String
fullGroupName gk = do
  ck <- courseOfGroup gk
  course <- loadCourse ck
  group <- loadGroup gk
  return $ concat [(courseName course), " - ", (groupName group)]

groupDescription :: GroupKey -> Persist (GroupKey, GroupDesc)
groupDescription gk = do
  name <- fullGroupName gk
  admins <- mapM (userDescription) =<< (groupAdmins gk)
  let gd = GroupDesc {
    gName   = name
  , gAdmins = map ud_fullname admins
  }
  return (gk,gd)

submissionDesc :: SubmissionKey -> Persist SubmissionDesc
submissionDesc sk = do
  submission <- loadSubmission sk
  un <- usernameOfSubmission sk
  u  <- u_name <$> loadUser un
  ak <- assignmentOfSubmission sk
  asg <- loadAssignment ak
  created <- assignmentCreatedTime ak
  cgk <- courseOrGroupOfAssignment ak
  cs  <- mapM loadComment =<< (commentsOfSubmission sk)
  fs  <- mapM loadFeedback =<< (feedbacksOfSubmission sk)
  case cgk of
    Left ck  -> do
      course <- loadCourse ck
      return SubmissionDesc {
          eCourse   = courseName course
        , eGroup    = Nothing
        , eStudent  = u
        , eUsername = un
        , eSolution = submissionValue id (const "zipped") (solution submission)
        , eAssignment     = asg
        , eAssignmentKey  = ak
        , eAssignmentDate = created
        , eSubmissionDate = solutionPostDate submission
        , eComments = cs
        , eFeedbacks = fs
        }
    Right gk -> do
      group <- loadGroup gk
      let gname = groupName group
      ck   <- courseOfGroup gk
      cname <- courseName <$> loadCourse ck
      return SubmissionDesc {
          eCourse   = cname
        , eGroup    = Just $ groupName group
        , eStudent  = u
        , eUsername = un
        , eSolution = submissionValue id (const "zipped") (solution submission)
        , eAssignment     = asg
        , eAssignmentKey  = ak
        , eAssignmentDate = created
        , eSubmissionDate = solutionPostDate submission
        , eComments = cs
        , eFeedbacks = fs
        }

-- Calculates the opened submissions for the user from the administrated groups and courses
openedSubmissionInfo :: Username -> Persist OpenedSubmissions
openedSubmissionInfo u = do
  acs <- map fst <$> administratedCourses u
  ags <- map fst <$> administratedGroups  u
  agcs <- (\\ acs) <$> mapM courseOfGroup ags
  isCourseAsg <- (flip elem . concat) <$> mapM courseAssignments acs
  isGroupAsg  <- (flip elem . concat) <$> mapM groupAssignments  ags
  isRelatedCourseAsg <- (flip elem . concat) <$> mapM courseAssignments agcs
  courseUser  <- (Set.fromList . concat) <$> mapM subscribedToCourse acs
  isGroupUser <- (flip Set.member . Set.fromList . concat) <$> mapM subscribedToGroup ags
  isRelatedCourseUser <- (flip Set.member . flip Set.difference courseUser . Set.fromList . concat)
                            <$> mapM subscribedToCourse agcs
  let isCourseUser = flip Set.member courseUser
  nonEvalSubs  <- openedSubmissions -- Non Evaluated Submissions
  submissionDescs <- mapM (withKey submissionDesc) nonEvalSubs
  let filterSubmissions os s@(_sk,sd) =
        let separate ak student
              | (isRelatedCourseAsg ak && isGroupUser student)  = os { osAdminedCourse = s:osAdminedCourse os }
              | (isCourseAsg ak && isGroupUser student)  = os { osAdminedCourse = s:osAdminedCourse os }
              | (isGroupAsg ak && isGroupUser student)   = os { osAdminedGroup  = s:osAdminedGroup os  }
              | (isRelatedCourseAsg ak && isRelatedCourseUser student) = os { osRelatedCourse = s:osRelatedCourse os }
              | (isCourseAsg ak && isRelatedCourseUser student) = os { osRelatedCourse = s:osRelatedCourse os }
              | (isCourseAsg ak && isCourseUser student) = os { osRelatedCourse = s:osRelatedCourse os }
              | otherwise = os
        in separate (eAssignmentKey sd) (eUsername sd)
  return $ foldl filterSubmissions empty submissionDescs
    where
      empty = OpenedSubmissions [] [] []
      withKey m k = do
        x <- m k
        return (k,x)

courseNameAndAdmins :: AssignmentKey -> Persist (CourseName, [UsersFullname])
courseNameAndAdmins ak = do
  eCkGk <- courseOrGroupOfAssignment ak
  (name, admins) <- case eCkGk of
    Left  ck -> do
      name   <- courseName <$> loadCourse ck
      admins <- courseAdmins ck
      return (name, admins)
    Right gk -> do
      name   <- fullGroupName gk
      admins <- groupAdmins gk
      return (name, admins)
  adminNames <- mapM (fmap ud_fullname . userDescription) admins
  return (name, adminNames)


submissionListDesc :: Username -> AssignmentKey -> Persist SubmissionListDesc
submissionListDesc u ak = do
  (name, adminNames) <- courseNameAndAdmins ak
  asg <- loadAssignment ak
  now <- liftIO getCurrentTime

  -- User submissions should not shown for urn typed assignments, only after the end
  -- period
  submissions <- do
    us <- userSubmissions u ak
    let aspects = Assignment.aspects asg
    if Assignment.isBallotBox aspects
      then if Assignment.end asg < now
             then Right <$> (mapM submissionStatus us)
             else Left  <$> (mapM submissionTime   us)
      else Right <$> (mapM submissionStatus us)

  return SubmissionListDesc {
    slGroup = name
  , slTeacher = adminNames
  , slAssignment = asg
  , slSubmissions = submissions
  }
  where
    submissionStatus sk = do
      time <- solutionPostDate <$> loadSubmission sk
      si <- submissionInfo sk
      return (sk, time, si, "TODO: EvaluatedBy")

    submissionTime sk = solutionPostDate <$> loadSubmission sk

submissionEvalStr :: SubmissionKey -> Persist (Maybe String)
submissionEvalStr sk = do
  mEk <- evaluationOfSubmission sk
  case mEk of
    Nothing -> return Nothing
    Just ek -> eString <$> loadEvaluation ek
  where
    eString = Just . translateMessage trans . resultString . evaluationResult

submissionDetailsDesc :: SubmissionKey -> Persist SubmissionDetailsDesc
submissionDetailsDesc sk = do
  ak <- assignmentOfSubmission sk
  (name, adminNames) <- courseNameAndAdmins ak
  asg <- loadAssignment ak
  sol <- solution       <$> loadSubmission sk
  cs  <- mapM loadComment =<< (commentsOfSubmission sk)
  fs  <- mapM loadFeedback =<< (feedbacksOfSubmission sk)
  s   <- submissionEvalStr sk
  return SubmissionDetailsDesc {
    sdGroup   = name
  , sdTeacher = adminNames
  , sdAssignment = asg
  , sdStatus     = s
  , sdSubmission = submissionValue id (const "zipped") sol
  , sdComments   = cs
  , sdFeedbacks  = fs
  }

-- | Checks if the assignment of the submission is adminstrated by the user
isAdminedSubmission :: Username -> SubmissionKey -> Persist Bool
isAdminedSubmission u sk = do
  -- Assignment of the submission
  ak <- assignmentOfSubmission sk

  -- Assignment Course Key
  ack <- either return (courseOfGroup) =<< (courseOrGroupOfAssignment ak)

  -- All administrated courses
  groupCourses <- mapM (courseOfGroup . fst) =<< (administratedGroups u)
  courses <- map fst <$> administratedCourses u
  let allCourses = nub (groupCourses ++ courses)

  return $ elem ack allCourses


-- TODO
canUserCommentOn :: Username -> SubmissionKey -> Persist Bool
canUserCommentOn _u _sk = return True

-- Returns all the submissions of the users for the groups that the
-- user administrates
submissionTables :: Username -> Persist [SubmissionTableInfo]
submissionTables u = do
  groupKeys <- map fst <$> administratedGroups u
  groupTables  <- mapM (groupSubmissionTableInfo) groupKeys
  return groupTables

groupSubmissionTableInfo :: GroupKey -> Persist SubmissionTableInfo
groupSubmissionTableInfo gk = do
  ck <- courseOfGroup gk
  gassignments <- groupAssignments gk
  cassignments <- courseAssignments ck
  usernames   <- subscribedToGroup gk
  name <- fullGroupName gk
  mkGroupSubmissionTableInfo name usernames cassignments gassignments ck gk

-- Returns the course submission table information for the given course key
courseSubmissionTableInfo :: CourseKey -> Persist SubmissionTableInfo
courseSubmissionTableInfo ck = do
  assignments <- courseAssignments ck
  usernames   <- subscribedToCourse ck
  name        <- courseName <$> loadCourse ck
  mkCourseSubmissionTableInfo name usernames assignments ck

-- Sort the given keys into an ordered list based on the time function
sortKeysByTime :: (key -> Persist UTCTime) -> [key] -> Persist [key]
sortKeysByTime time keys = map snd . sortBy (compare `on` fst) <$> mapM getTime keys
  where
    getTime k = do
      t <- time k
      return (t,k)

loadAssignmentInfos :: [AssignmentKey] -> Persist (Map AssignmentKey Assignment)
loadAssignmentInfos as = Map.fromList <$> mapM loadAssignmentInfo as
  where
    loadAssignmentInfo a = do
       asg <- loadAssignment a
       return (a,asg)

submissionInfoAsgKey :: Username -> AssignmentKey -> Persist (AssignmentKey, SubmissionInfo)
submissionInfoAsgKey u ak = addKey <$> (userLastSubmissionInfo u ak)
  where
    addKey s = (ak,s)

-- TODO: Need to be add semantics there
calculateResult :: [SubmissionInfo] -> Maybe Result
calculateResult _ = Nothing

#ifdef TEST

calculateResultTests = do
  eqPartitions calculateResult
    [ ( "Empty list", [], Nothing, "")

    , ( "One binary submission", [Submission_Result undefined (binaryResult Passed)]
      , Just Passed, "")

    , ( "One percentage submission", [Submission_Result undefined (percentageResult 0.1)]
      , Just Passed, "")
    ]

#endif

mkCourseSubmissionTableInfo
  :: String -> [Username] -> [AssignmentKey] -> CourseKey
  -> Persist SubmissionTableInfo
mkCourseSubmissionTableInfo courseName us as key = do
  assignments <- sortKeysByTime assignmentCreatedTime as
  assignmentInfos <- loadAssignmentInfos as
  ulines <- forM us $ \u -> do
    ud <- userDescription u
    asi <- mapM (submissionInfoAsgKey u) as
    let result = case asi of
                   [] -> Nothing
                   _  -> calculateResult $ map snd asi
    return (ud, result, Map.fromList asi)
  return CourseSubmissionTableInfo {
      stiCourse = courseName
    , stiUsers = us
    , stiAssignments = assignments
    , stiUserLines = ulines
    , stiAssignmentInfos = assignmentInfos
    , stiCourseKey = key
    }

mkGroupSubmissionTableInfo
  :: String
  -> [Username] -> [AssignmentKey] -> [AssignmentKey]
  -> CourseKey -> GroupKey
  -> Persist SubmissionTableInfo
mkGroupSubmissionTableInfo courseName us cas gas ckey gkey = do
  cgAssignments   <- sortKeysByTime createdTime ((map CourseInfo cas) ++ (map GroupInfo gas))
  assignmentInfos <- loadAssignmentInfos (cas ++ gas)
  ulines <- forM us $ \u -> do
    ud <- userDescription u
    casi <- mapM (submissionInfoAsgKey u) cas
    gasi <- mapM (submissionInfoAsgKey u) gas
    let result = case gasi of
                   [] -> Nothing
                   _  -> calculateResult $ map snd gasi
    return (ud, result, Map.fromList (casi ++ gasi))
  return GroupSubmissionTableInfo {
      stiCourse = courseName
    , stiUsers = us
    , stiCGAssignments = cgAssignments
    , stiUserLines = ulines
    , stiAssignmentInfos = assignmentInfos
    , stiCourseKey = ckey
    , stiGroupKey  = gkey
    }
  where
    createdTime = cgInfoCata
      (assignmentCreatedTime)
      (assignmentCreatedTime)

submissionInfo :: SubmissionKey -> Persist SubmissionInfo
submissionInfo sk = do
  mEk <- evaluationOfSubmission sk
  case mEk of
    Nothing -> do
      fs <- mapM loadFeedback =<< (feedbacksOfSubmission sk)
      -- Supposing that only one test result feedback will arrive
      -- to a submission.
      return . maybe
        Submission_Unevaluated
        Submission_Tested
          $ feedbackTestResult =<< find isTestedFeedback fs
    Just ek -> (Submission_Result ek . evaluationResult) <$> loadEvaluation ek

-- Produces information of the last submission for the given user and assignment
userLastSubmissionInfo :: Username -> AssignmentKey -> Persist SubmissionInfo
userLastSubmissionInfo u ak =
  (maybe (return Submission_Not_Found) (submissionInfo)) =<< (lastSubmission ak u)

userSubmissionDesc :: Username -> AssignmentKey -> Persist UserSubmissionDesc
userSubmissionDesc u ak = do
  -- Calculate the normal fields
  asgName       <- Assignment.name <$> loadAssignment ak
  courseOrGroup <- courseOrGroupOfAssignment ak
  crName <- case courseOrGroup of
              Left  ck -> courseName <$> loadCourse ck
              Right gk -> fullGroupName gk
  student <- ud_fullname <$> userDescription u
  keys    <- userSubmissions u ak
  -- Calculate the submission information list
  submissions <- flip mapM keys $ \sk -> do
    time  <- solutionPostDate <$> loadSubmission sk
    sinfo <- submissionInfo sk
    return (sk, time, sinfo)

  return UserSubmissionDesc {
    usCourse         = crName
  , usAssignmentName = asgName
  , usStudent        = student
  , usSubmissions    = submissions
  }

-- Helper computation which removes the given submission from
-- the opened submission directory, which is optimized by
-- assignment and username keys, for the quickier lookup
removeOpenedSubmission :: SubmissionKey -> Persist ()
removeOpenedSubmission sk = do
  ak <- assignmentOfSubmission sk
  u  <- usernameOfSubmission sk
  removeFromOpened ak u sk

-- Make unsibscribe a user from a course if the user attends in the course
-- otherwise do nothing
deleteUserFromCourse :: CourseKey -> Username -> Persist ()
deleteUserFromCourse ck u = do
  cs <- userCourses u
  when (ck `elem` cs) $ do
    gs <- userGroups u
    -- Collects all the courses for the user's group
    cgMap <- Map.fromList <$> (forM gs $ runKleisli ((k (courseOfGroup)) &&& (k return)))
    -- Unsubscribe the user from a given course with the found group
    maybe
      (return ()) -- TODO: Logging should be usefull
      (unsubscribe u ck)
      (Map.lookup ck cgMap)
  where
    k = Kleisli

testScriptInfo :: TestScriptKey -> Persist TestScriptInfo
testScriptInfo tk = do
  script <- loadTestScript tk
  return TestScriptInfo {
      tsiName = tsName script
    , tsiDescription = tsDescription script
    , tsiType = tsType script
    }

-- Returns True if the given student submitted at least one solution for the
-- assignments for the given group, otherwise False
isThereASubmissionForGroup :: Username -> GroupKey -> Persist Bool
isThereASubmissionForGroup u gk = do
  aks <- groupAssignments gk
  (not . null . catMaybes) <$> mapM (flip (lastSubmission) u) aks

-- Returns True if the given student submitted at least one solution for the
-- assignments for the given group, otherwise False
isThereASubmissionForCourse :: Username -> CourseKey -> Persist Bool
isThereASubmissionForCourse u ck = do
  aks <- courseAssignments ck
  (not . null . catMaybes) <$> mapM (flip (lastSubmission) u) aks

-- Returns the number of the possible submission for the given assignment
-- by the given user.
submissionLimitOfAssignment :: Username -> AssignmentKey -> Persist SubmissionLimit
submissionLimitOfAssignment username key =
  calcSubLimit <$> (loadAssignment key) <*> (length <$> userSubmissions username key)

#ifdef TEST

dbStep = Keyword.step . Keyword.key

persistRelationsTests = do
  userAssignmentKeysTest
  calculateResultTests

#endif
