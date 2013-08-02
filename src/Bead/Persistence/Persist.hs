module Bead.Persistence.Persist (
    Persist(..)
  , runPersist
  , userAssignmentKeys
  , submissionDesc
  , submissionListDesc
  , submissionDetailsDesc
  , groupDescription
  , isAdminedSubmission
  , canUserCommentOn
  , submissionTables
  , userSubmissionDesc
  , courseOrGroupOfAssignment
  , courseNameAndAdmins
  ) where

import Bead.Domain.Types (Erroneous)
import Bead.Domain.Entities
import Bead.Domain.Relationships

import Data.Function (on)
import Data.Time (UTCTime)
import Data.List (nub, sortBy)
import Data.Map (Map(..))
import qualified Data.Map as Map

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (mapM, liftM, liftM2)
import Control.Exception (IOException)
import Control.Monad.Transaction.TIO

data Persist = Persist {
  -- User Persistence
    saveUser      :: User -> Password -> TIO ()
  , canUserLogin  :: Username -> Password -> TIO Bool
  , personalInfo  :: Username -> Password -> TIO (Role, String)
  , updatePwd     :: Username -> Password -> Password -> TIO ()
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
  , groupAdmins   :: GroupKey -> TIO [Username]
  , createGroupProfessor :: Username -> GroupKey -> TIO ()
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
  , evaulationOfSubmission :: SubmissionKey -> TIO (Maybe EvaulationKey)
  , commentsOfSubmission :: SubmissionKey -> TIO [CommentKey]
  , lastSubmission :: AssignmentKey -> Username -> TIO (Maybe SubmissionKey)

  , placeToOpened     :: SubmissionKey -> TIO ()
  , removeFromOpened  :: SubmissionKey -> TIO ()
  , openedSubmissions :: TIO [SubmissionKey]

  -- Evaulation
  , saveEvaulation :: SubmissionKey -> Evaulation -> TIO EvaulationKey
  , loadEvaulation :: EvaulationKey -> TIO Evaulation
  , modifyEvaulation :: EvaulationKey -> Evaulation -> TIO ()
  , submissionOfEvaulation :: EvaulationKey -> TIO SubmissionKey

  -- Comment
  , saveComment :: SubmissionKey -> Comment -> TIO CommentKey
  , loadComment :: CommentKey -> TIO Comment
  , submissionOfComment :: CommentKey -> TIO SubmissionKey

  -- Persistence initialization
  , isPersistenceSetUp :: IO Bool
  , initPersistence    :: IO ()
  }

-- * Combined Persistence Tasks

userAssignmentKeys :: Persist -> Username -> TIO [AssignmentKey]
userAssignmentKeys p u = do
  gs <- userGroups p u
  cs <- userCourses p u
  asg <- concat <$> (mapM (groupAssignments p)  (nub gs))
  asc <- concat <$> (mapM (courseAssignments p) (nub cs))
  return . nub $ (asg ++ asc)

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

groupDescription :: Persist -> GroupKey -> TIO (GroupKey, GroupDesc)
groupDescription p gk = do
  g  <- loadGroup p gk
  admins <- mapM (userDescription p) =<< (groupAdmins p gk)
  let gd = GroupDesc {
    gName   = groupName g
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
    Right gk -> (groupEvalConfig  &&& groupName)  <$> loadGroup  p gk
  cs  <- mapM (loadComment p) =<< (commentsOfSubmission p sk)
  return SubmissionDesc {
    eGroup    = gr
  , eStudent  = u
  , eSolution = s
  , eConfig = c
  , eAssignmentTitle = assignmentName asg
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
      name   <- groupName  <$> loadGroup  p gk
      admins <- groupAdmins p gk
      return (name, admins)
  adminNames <- mapM (fmap ud_fullname . userDescription p) admins
  return (name, adminNames)



submissionListDesc :: Persist -> Username -> AssignmentKey -> TIO SubmissionListDesc
submissionListDesc p u ak = do
  (name, adminNames) <- courseNameAndAdmins p ak
  asg <- loadAssignment p ak
  statuses <- mapM submissionStatus =<< userSubmissions p u ak
  return SubmissionListDesc {
    slGroup = name
  , slTeacher = adminNames
  , slSubmissions = statuses
  , slAssignmentText = assignmentDesc asg
  }
  where
    submissionStatus sk = do
      time <- solutionPostDate <$> loadSubmission p sk
      s <- submissionEvalStr p sk
      return (sk, time, s, "TODO: EvaulatedBy")

submissionEvalStr :: Persist -> SubmissionKey -> TIO String
submissionEvalStr p sk = do
  mEk <- evaulationOfSubmission p sk
  case mEk of
    Nothing -> return "Not evaulated yet"
    Just ek -> eString <$> loadEvaulation p ek
  where
    eString = resultString . evaulationResult

submissionDetailsDesc :: Persist -> SubmissionKey -> TIO SubmissionDetailsDesc
submissionDetailsDesc p sk = do
  ak <- assignmentOfSubmission p sk
  (name, adminNames) <- courseNameAndAdmins p ak
  asg <- assignmentDesc <$> loadAssignment p ak
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

-- TODO
-- | Checks if the assignment of the submission is adminstrated by the user
isAdminedSubmission :: Persist -> Username -> SubmissionKey -> TIO Bool
isAdminedSubmission p u sk = return True

-- TODO
canUserCommentOn :: Persist -> Username -> SubmissionKey -> TIO Bool
canUserCommentOn p u sk = return True

submissionTables :: Persist -> Username -> TIO [SubmissionTableInfo]
submissionTables p u = do
  courseTables <- mapM (courseSubmissionTableInfo p . fst) =<< administratedCourses p u
  groupTables  <- mapM (groupSubmissionTableInfo  p . fst) =<< administratedGroups  p u
  return $ courseTables ++ groupTables

groupSubmissionTableInfo :: Persist -> GroupKey -> TIO SubmissionTableInfo
groupSubmissionTableInfo p gk = do
  assignments <- groupAssignments p gk
  usernames   <- subscribedToGroup p gk
  (name,evalCfg) <- (groupName &&& groupEvalConfig) <$> loadGroup p gk
  submissionTableInfo p name evalCfg assignments usernames

courseSubmissionTableInfo :: Persist -> CourseKey -> TIO SubmissionTableInfo
courseSubmissionTableInfo p ck = do
  assignments <- courseAssignments p ck
  usernames   <- subscribedToCourse p ck
  (name,evalCfg) <- (courseName &&& courseEvalConfig) <$> loadCourse p ck
  submissionTableInfo p name evalCfg assignments usernames

submissionTableInfo
  :: Persist
  -> String
  -> EvaulationConfig
  -> [AssignmentKey]
  -> [Username]
  -> TIO SubmissionTableInfo
submissionTableInfo p courseName evalCfg as usernames = do
  assignments <- sortAssignments as

  ulines <- flip mapM usernames $ \u -> do
    ud  <- userDescription p u
    asi <- mapM (submissionInfo' u) as
    let result = calculateResult . map snd $ asi
    return (ud, result, asi)

  return SubmissionTableInfo {
    stCourse      = courseName
  , stNumberOfAssignments = length assignments
  , stEvalConfig  = evalCfg
  , stAssignments = assignments
  , stUsers       = usernames
  , stUserLines   = ulines
  }
  where
    sortAssignments :: [AssignmentKey] -> TIO [AssignmentKey]
    sortAssignments ks = map snd . sortBy (compare `on` fst) <$> mapM (assignmentCreatedTime') ks

    assignmentCreatedTime' k = do
      t <- assignmentCreatedTime p k
      return (t,k)

    submissionInfo' u ak = do
      s <- do mSk <- lastSubmission p ak u
              case mSk of
                Nothing -> return Submission_Not_Found
                Just sk -> submissionInfo p sk
      return (ak,s)

    calculateResult = evaulateResults evalCfg . map sbmResult . filter hasResult

    hasResult (Submission_Result _ _) = True
    hasResult _                       = False

    sbmResult (Submission_Result _ r) = r
    sbmResult _ = error "sbmResult: impossible"

submissionInfo :: Persist -> SubmissionKey -> TIO SubmissionInfo
submissionInfo p sk = do
  mEk <- evaulationOfSubmission p sk
  case mEk of
    Nothing -> return Submission_Unevaulated
    Just ek -> (Submission_Result ek . evaulationResult) <$> loadEvaulation p ek

userSubmissionDesc :: Persist -> Username -> AssignmentKey -> TIO UserSubmissionDesc
userSubmissionDesc p u ak = do
  -- Calculate the normal fields
  asgName       <- assignmentName <$> loadAssignment p ak
  courseOrGroup <- courseOrGroupOfAssignment p ak
  crName <- case courseOrGroup of
              Left  ck -> courseName <$> loadCourse p ck
              Right gk -> groupName  <$> loadGroup  p gk
  student <- ud_fullname <$> userDescription p u
  keys    <- userSubmissions p u ak
  -- Calculate the submission information list
  submissions <- flip mapM keys $ \sk -> do
    time  <- solutionPostDate <$> loadSubmission p sk
    sinfo <- submissionInfo p sk
    return (sk, time, sinfo, EvHand)

  return UserSubmissionDesc {
    usCourse         = crName
  , usAssignmentName = asgName
  , usStudent        = student
  , usSubmissions    = submissions
  }

-- * Runner Tools

reason :: Either IOException a -> (Erroneous a)
reason (Left e)  = Left . show $ e
reason (Right x) = Right x

runPersist :: TIO a -> IO (Erroneous a)
runPersist = liftM reason . atomically

