module Bead.Persistence.Persist (
    Persist(..)
  , runPersist
  , userAssignmentKeys
  , userAssignmentKeyList
  , submissionDesc
  , submissionListDesc
  , submissionDetailsDesc
  , groupDescription
  , isAdminedSubmission
  , canUserCommentOn
  , submissionTables
  , userSubmissionDesc
  , userLastSubmissionInfo
  , courseOrGroupOfAssignment
  , courseNameAndAdmins
  , administratedGroupsWithCourseName
  , groupsOfUsersCourse
  , removeOpenedSubmission
  , deleteUserFromCourse -- Deletes a user from a course, searching the group id for the unsubscription
  ) where

import Bead.Domain.Types (Erroneous)
import Bead.Domain.Entities
import Bead.Domain.Relationships

import Data.Function (on)
import Data.Time (UTCTime)
import Data.List (nub, sortBy, intersect)
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Time (getCurrentTime)

import Control.Applicative ((<$>))
import Control.Arrow
import Control.Monad (mapM, liftM, liftM2, forM, when)
import Control.Exception (IOException)
import Control.Monad.Transaction.TIO

data Persist = Persist {
  -- User Persistence
    saveUser      :: User -> TIO ()
  , personalInfo  :: Username -> TIO PersonalInfo -- (Role, String)
  , filterUsers   :: (User -> Bool) -> TIO [User]
  , loadUser      :: Username -> TIO User
  , updateUser    :: User -> TIO ()
  , doesUserExist :: Username -> TIO Bool
  , userDescription :: Username -> TIO UserDesc
  , userSubmissions :: Username -> AssignmentKey -> TIO [SubmissionKey]
  , administratedCourses :: Username -> TIO [(CourseKey, Course)]
  , administratedGroups  :: Username -> TIO [(GroupKey, Group)]

  -- Registration
  , saveUserReg   :: UserRegistration -> TIO UserRegKey
  , loadUserReg   :: UserRegKey -> TIO UserRegistration

  -- Course Persistence
  , saveCourse        :: Course -> TIO CourseKey
  , courseKeys        :: TIO [CourseKey]
  , filterCourses     :: (CourseKey -> Course -> Bool) -> TIO [(CourseKey, Course)]
  , loadCourse        :: CourseKey -> TIO Course
  , groupKeysOfCourse :: CourseKey -> TIO [GroupKey]
  , isUserInCourse    :: Username -> CourseKey -> TIO Bool
  , userCourses       :: Username -> TIO [CourseKey]
  , createCourseAdmin :: Username -> CourseKey -> TIO ()
  , courseAdmins      :: CourseKey -> TIO [Username]
  , subscribedToCourse :: CourseKey -> TIO [Username]

  -- Group Persistence
  , saveGroup     :: CourseKey -> Group -> TIO GroupKey
  , loadGroup     :: GroupKey -> TIO Group
  , courseOfGroup :: GroupKey -> TIO CourseKey
  , filterGroups  :: (GroupKey -> Group -> Bool) -> TIO [(GroupKey, Group)]
  , isUserInGroup :: Username -> GroupKey -> TIO Bool
  , userGroups    :: Username -> TIO [GroupKey]
  , subscribe     :: Username -> CourseKey -> GroupKey -> TIO ()
  , unsubscribe   :: Username -> CourseKey -> GroupKey -> TIO ()
  , groupAdmins   :: GroupKey -> TIO [Username]
  , createGroupAdmin  :: Username -> GroupKey -> TIO ()
  , subscribedToGroup    :: GroupKey -> TIO [Username]

  -- Assignment Persistence
  , filterAssignment  :: (AssignmentKey -> Assignment -> Bool) -> TIO [(AssignmentKey, Assignment)]
  , assignmentKeys    :: TIO [AssignmentKey]
  , saveAssignment    :: Assignment -> TIO AssignmentKey
  , loadAssignment    :: AssignmentKey -> TIO Assignment
  , modifyAssignment  :: AssignmentKey -> Assignment -> TIO ()
  , courseAssignments :: CourseKey -> TIO [AssignmentKey]
  , groupAssignments  :: GroupKey -> TIO [AssignmentKey]
  , saveCourseAssignment :: CourseKey -> Assignment -> TIO AssignmentKey
  , saveGroupAssignment  :: GroupKey  -> Assignment -> TIO AssignmentKey
  , courseOfAssignment   :: AssignmentKey -> TIO (Maybe CourseKey)
  , groupOfAssignment    :: AssignmentKey -> TIO (Maybe GroupKey)
  , submissionsForAssignment :: AssignmentKey -> TIO [SubmissionKey]
  , assignmentCreatedTime    :: AssignmentKey -> TIO UTCTime

  -- Submission
  , saveSubmission :: AssignmentKey -> Username -> Submission -> TIO SubmissionKey
  , loadSubmission :: SubmissionKey -> TIO Submission
  , assignmentOfSubmission :: SubmissionKey -> TIO AssignmentKey
  , usernameOfSubmission   :: SubmissionKey -> TIO Username
  , filterSubmissions :: (SubmissionKey -> Submission -> Bool) -> TIO [(SubmissionKey, Submission)]
  , evaluationOfSubmission :: SubmissionKey -> TIO (Maybe EvaluationKey)
  , commentsOfSubmission :: SubmissionKey -> TIO [CommentKey]
  , lastSubmission :: AssignmentKey -> Username -> TIO (Maybe SubmissionKey)

  , removeFromOpened  :: AssignmentKey -> Username -> SubmissionKey -> TIO ()
  , openedSubmissions :: TIO [SubmissionKey]
  , usersOpenedSubmissions :: AssignmentKey -> Username -> TIO [SubmissionKey]
  -- ^ Calculates all the opened submisison for a given user and a given assignment

  -- Evaluation
  , saveEvaluation :: SubmissionKey -> Evaluation -> TIO EvaluationKey
  , loadEvaluation :: EvaluationKey -> TIO Evaluation
  , modifyEvaluation :: EvaluationKey -> Evaluation -> TIO ()
  , submissionOfEvaluation :: EvaluationKey -> TIO SubmissionKey

  -- Comment
  , saveComment :: SubmissionKey -> Comment -> TIO CommentKey
  , loadComment :: CommentKey -> TIO Comment
  , submissionOfComment :: CommentKey -> TIO SubmissionKey

  -- Persistence initialization
  , isPersistenceSetUp :: IO Bool
  , initPersistence    :: IO ()
  }

-- * Combined Persistence Tasks

-- Computes the Group key list, which should contain one element,
-- for a course key and a user, which the user attends in.
groupsOfUsersCourse :: Persist -> Username -> CourseKey -> TIO [GroupKey]
groupsOfUsersCourse p u ck = do
  ugs <- nub <$> userGroups p u
  cgs <- nub <$> groupKeysOfCourse p ck
  return $ intersect ugs cgs

-- Produces a Just Assignment list, if the user is registered for some courses,
-- otherwise Nothing.
userAssignmentKeys :: Persist -> Username -> TIO (Maybe [AssignmentKey])
userAssignmentKeys p u = do
  gs <- userGroups p u
  cs <- userCourses p u
  case (cs,gs) of
    ([],[]) -> return Nothing
    _       -> do
      asg <- concat <$> (mapM (groupAssignments p)  (nub gs))
      asc <- concat <$> (mapM (courseAssignments p) (nub cs))
      return . Just $ nub (asg ++ asc)

-- Produces the assignment key list for the user, it the user
-- is not registered in any course the result is the empty list
userAssignmentKeyList :: Persist -> Username -> TIO [AssignmentKey]
userAssignmentKeyList p u = (maybe [] id) <$> (userAssignmentKeys p u)

courseOrGroupOfAssignment :: Persist -> AssignmentKey -> TIO (Either CourseKey GroupKey)
courseOrGroupOfAssignment p ak = do
  mGk <- groupOfAssignment p ak
  case mGk of
    Just gk -> return . Right $ gk
    Nothing -> do
      mCk <- courseOfAssignment p ak
      case mCk of
        Just ck -> return . Left $ ck
        Nothing -> error $ "Impossible: No course or groupkey was found for the assignment:" ++ show ak

administratedGroupsWithCourseName :: Persist -> Username -> TIO [(GroupKey, Group, String)]
administratedGroupsWithCourseName p u = do
  gs <- administratedGroups p u
  forM gs $ \(gk,g) -> do
    fn <- fullGroupName p gk
    return (gk,g,fn)

-- Produces a full name for a group including the name of the course.
fullGroupName :: Persist -> GroupKey -> TIO String
fullGroupName p gk = do
  ck <- courseOfGroup p gk
  course <- loadCourse p ck
  group <- loadGroup p gk
  return $ concat [(courseName course), " - ", (groupName group)]

groupDescription :: Persist -> GroupKey -> TIO (GroupKey, GroupDesc)
groupDescription p gk = do
  name <- fullGroupName p gk
  admins <- mapM (userDescription p) =<< (groupAdmins p gk)
  let gd = GroupDesc {
    gName   = name
  , gAdmins = map ud_fullname admins
  }
  return (gk,gd)

submissionDesc :: Persist -> SubmissionKey -> TIO SubmissionDesc
submissionDesc p sk = do
  s  <- solution <$> loadSubmission p sk
  un <- usernameOfSubmission p sk
  u  <- u_name <$> loadUser p un
  ak <- assignmentOfSubmission p sk
  asg <- loadAssignment p ak
  cgk <- courseOrGroupOfAssignment p ak
  (c,gr) <- case cgk of
    Left ck  -> (courseEvalConfig &&& courseName) <$> loadCourse p ck
    Right gk -> do
      cfg  <- groupEvalConfig <$> loadGroup p gk
      name <- fullGroupName p gk
      return (cfg, name)
  cs  <- mapM (loadComment p) =<< (commentsOfSubmission p sk)
  return SubmissionDesc {
    eGroup    = gr
  , eStudent  = u
  , eSolution = s
  , eConfig = c
  , eAssignmentKey   = ak
  , eAssignmentTitle = assignmentName asg
  , eAssignmentDesc  = assignmentDesc asg
  , eComments = cs
  }

courseNameAndAdmins :: Persist -> AssignmentKey -> TIO (CourseName, [UsersFullname])
courseNameAndAdmins p ak = do
  eCkGk <- courseOrGroupOfAssignment p ak
  (name, admins) <- case eCkGk of
    Left  ck -> do
      name   <- courseName <$> loadCourse p ck
      admins <- courseAdmins p ck
      return (name, admins)
    Right gk -> do
      name   <- fullGroupName p gk
      admins <- groupAdmins p gk
      return (name, admins)
  adminNames <- mapM (fmap ud_fullname . userDescription p) admins
  return (name, adminNames)


submissionListDesc :: Persist -> Username -> AssignmentKey -> TIO SubmissionListDesc
submissionListDesc p u ak = do
  (name, adminNames) <- courseNameAndAdmins p ak
  asg <- loadAssignment p ak
  now <- hasNoRollback getCurrentTime

  -- User submissions should not shown for urn typed assignments, only after the end
  -- period
  submissions <- assignmentTypeCata
    -- Normal assignment
    (Right <$> (mapM submissionStatus =<< userSubmissions p u ak))
    -- Urn assignment
    (case (assignmentEnd asg < now) of
       True  -> Right <$> (mapM submissionStatus =<< userSubmissions p u ak)
       False -> Left  <$> (mapM submissionTime =<< userSubmissions p u ak))
    (assignmentType asg)

  return SubmissionListDesc {
    slGroup = name
  , slTeacher = adminNames
  , slAssignment = asg
  , slSubmissions = submissions
  }
  where
    submissionStatus sk = do
      time <- solutionPostDate <$> loadSubmission p sk
      s <- submissionEvalStr p sk
      return (sk, time, s, "TODO: EvaluatedBy")

    submissionTime sk = solutionPostDate <$> loadSubmission p sk

submissionEvalStr :: Persist -> SubmissionKey -> TIO String
submissionEvalStr p sk = do
  mEk <- evaluationOfSubmission p sk
  case mEk of
    Nothing -> return "Not evaluated yet"
    Just ek -> eString <$> loadEvaluation p ek
  where
    eString = resultString . evaluationResult

submissionDetailsDesc :: Persist -> SubmissionKey -> TIO SubmissionDetailsDesc
submissionDetailsDesc p sk = do
  ak <- assignmentOfSubmission p sk
  (name, adminNames) <- courseNameAndAdmins p ak
  asg <- loadAssignment p ak
  sol <- solution       <$> loadSubmission p sk
  cs  <- mapM (loadComment p) =<< (commentsOfSubmission p sk)
  s   <- submissionEvalStr p sk
  return SubmissionDetailsDesc {
    sdGroup   = name
  , sdTeacher = adminNames
  , sdAssignment = asg
  , sdStatus     = s
  , sdSubmission = sol
  , sdComments   = cs
  }

-- | Checks if the assignment of the submission is adminstrated by the user
isAdminedSubmission :: Persist -> Username -> SubmissionKey -> TIO Bool
isAdminedSubmission p u sk = do
  -- Assignment of the submission
  ak <- assignmentOfSubmission p sk

  -- Assignment Course Key
  ack <- either return (courseOfGroup p) =<< (courseOrGroupOfAssignment p ak)

  -- All administrated courses
  groupCourses <- mapM (courseOfGroup p . fst) =<< (administratedGroups p u)
  courses <- map fst <$> administratedCourses p u
  let allCourses = nub (groupCourses ++ courses)

  return $ elem ack allCourses


-- TODO
canUserCommentOn :: Persist -> Username -> SubmissionKey -> TIO Bool
canUserCommentOn p u sk = return True

-- Returns all the submissions of the users for the groups and courses that the
-- user administrates
submissionTables :: Persist -> Username -> TIO [SubmissionTableInfo]
submissionTables p u = do
  courseKeys <- map fst <$> administratedCourses p u
  courseTables <- mapM (courseSubmissionTableInfo p) courseKeys
  groupKeys <- map fst <$> administratedGroups p u
  groupTables  <- mapM (groupSubmissionTableInfo p) groupKeys
  courseOfGroupTables <- catMaybes <$> mapM (courseSubmissionTableInfoForGroupAdmin p courseKeys) groupKeys
  return $ courseTables ++ courseOfGroupTables ++ groupTables

groupSubmissionTableInfo :: Persist -> GroupKey -> TIO SubmissionTableInfo
groupSubmissionTableInfo p gk = do
  assignments <- groupAssignments p gk
  usernames   <- subscribedToGroup p gk
  name <- fullGroupName p gk
  evalCfg <- groupEvalConfig <$> loadGroup p gk
  submissionTableInfo p name GroupInfSrc evalCfg assignments usernames (Left gk)

courseSubmissionTableInfo :: Persist -> CourseKey -> TIO SubmissionTableInfo
courseSubmissionTableInfo p ck = do
  assignments <- courseAssignments p ck
  usernames   <- subscribedToCourse p ck
  (name,evalCfg) <- (courseName &&& courseEvalConfig) <$> loadCourse p ck
  submissionTableInfo p name CourseInfSrc evalCfg assignments usernames (Right ck)

-- Produces a submission table information, which is Just info, for the courses expect that the user is already
-- administrates, otherwise Nothing
courseSubmissionTableInfoForGroupAdmin :: Persist -> [CourseKey] -> GroupKey -> TIO (Maybe SubmissionTableInfo)
courseSubmissionTableInfoForGroupAdmin p cks gk = do
  ck <- courseOfGroup p gk
  if elem ck cks
    then return Nothing
    else do
      usernames <- subscribedToGroup p gk
      assignments <- courseAssignments p ck
      (name, evalCfg) <- (courseName &&& courseEvalConfig) <$> loadCourse p ck
      Just <$> submissionTableInfo p name GroupAdminCourseInfSrc evalCfg assignments usernames (Right ck)

submissionTableInfo
  :: Persist
  -> String
  -> InfoSource
  -> EvaluationConfig
  -> [AssignmentKey]
  -> [Username]
  -> Either GroupKey CourseKey
  -> TIO SubmissionTableInfo
submissionTableInfo p courseName source evalCfg as usernames key = do
  assignments <- sortAssignments as
  assignmentNames <- loadAssignmentNames as

  ulines <- flip mapM usernames $ \u -> do
    ud  <- userDescription p u
    asi <- mapM (submissionInfo' u) as
    let result = case asi of
                   [] -> Nothing
                   _  -> calculateResult . map snd $ asi
    return (ud, result, asi)

  return SubmissionTableInfo {
    stCourse      = courseName
  , stOrigin      = source
  , stNumberOfAssignments = length assignments
  , stEvalConfig  = evalCfg
  , stAssignments = assignments
  , stUsers       = usernames
  , stUserLines   = ulines
  , stAssignmentNames = assignmentNames
  , stKey         = key
  }
  where
    sortAssignments :: [AssignmentKey] -> TIO [AssignmentKey]
    sortAssignments ks = map snd . sortBy (compare `on` fst) <$> mapM (assignmentCreatedTime') ks

    assignmentCreatedTime' k = do
      t <- assignmentCreatedTime p k
      return (t,k)

    submissionInfo' u ak = addKey <$> (userLastSubmissionInfo p u ak)
      where
        addKey s = (ak,s)

    loadAssignmentNames as = Map.fromList <$> mapM loadAssignmentName as

    loadAssignmentName a = do
      name <- assignmentName <$> loadAssignment p a
      return (a,name)

    calculateResult = evaluateResults evalCfg . map sbmResult . filter hasResult

    hasResult (Submission_Result _ _) = True
    hasResult _                       = False

    sbmResult (Submission_Result _ r) = r
    sbmResult _ = error "sbmResult: impossible"

submissionInfo :: Persist -> SubmissionKey -> TIO SubmissionInfo
submissionInfo p sk = do
  mEk <- evaluationOfSubmission p sk
  case mEk of
    Nothing -> return Submission_Unevaluated
    Just ek -> (Submission_Result ek . evaluationResult) <$> loadEvaluation p ek

-- Produces information of the last submission for the given user and assignment
userLastSubmissionInfo :: Persist -> Username -> AssignmentKey -> TIO SubmissionInfo
userLastSubmissionInfo p u ak =
  (maybe (return Submission_Not_Found) (submissionInfo p)) =<< (lastSubmission p ak u)

userSubmissionDesc :: Persist -> Username -> AssignmentKey -> TIO UserSubmissionDesc
userSubmissionDesc p u ak = do
  -- Calculate the normal fields
  asgName       <- assignmentName <$> loadAssignment p ak
  courseOrGroup <- courseOrGroupOfAssignment p ak
  crName <- case courseOrGroup of
              Left  ck -> courseName <$> loadCourse p ck
              Right gk -> fullGroupName p gk
  student <- ud_fullname <$> userDescription p u
  keys    <- userSubmissions p u ak
  -- Calculate the submission information list
  submissions <- flip mapM keys $ \sk -> do
    time  <- solutionPostDate <$> loadSubmission p sk
    sinfo <- submissionInfo p sk
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
removeOpenedSubmission :: Persist -> SubmissionKey -> TIO ()
removeOpenedSubmission p sk = do
  ak <- assignmentOfSubmission p sk
  u  <- usernameOfSubmission p sk
  removeFromOpened p ak u sk

-- Make unsibscribe a user from a course if the user attends in the course
-- otherwise do nothing
deleteUserFromCourse :: Persist -> CourseKey -> Username -> TIO ()
deleteUserFromCourse p ck u = do
  cs <- userCourses p u
  when (ck `elem` cs) $ do
    gs <- userGroups p u
    -- Collects all the courses for the user's group
    cgMap <- Map.fromList <$> (forM gs $ runKleisli ((k (courseOfGroup p)) &&& (k return)))
    -- Unsubscribe the user from a given course with the found group
    maybe
      (return ()) -- TODO: Logging should be usefull
      (unsubscribe p u ck)
      (Map.lookup ck cgMap)
  where
    k = Kleisli

-- * Runner Tools

reason :: Either IOException a -> (Erroneous a)
reason (Left e)  = Left . show $ e
reason (Right x) = Right x

runPersist :: TIO a -> IO (Erroneous a)
runPersist = liftM reason . atomically

