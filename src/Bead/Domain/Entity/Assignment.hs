{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Entity.Assignment (
    Aspect(..)
  , aspect
  , SubmissionType(..)
  , Aspects
  , fromAspects
  , toAspects
  , emptyAspects
  , fromList
  , aspectsFromList
  , isPasswordAspect
  , isBallotBoxAspect
  , getPassword
  , setPassword

  , isPasswordProtected
  , isBallotBox
  , isZippedSubmissions
  , setZippedSubmissions
  , clearZippedSubmissions

  , Assignment(..)
  , assignmentCata
  , withAssignment
  , assignmentAna
  , isActive


#ifdef TEST
  , assignmentTests
  , asgTests
#endif
  ) where

import           Control.Applicative
import           Data.Data
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime(..))

import           Bead.Domain.Shared.Evaluation

#ifdef TEST
import           Test.Themis.Test hiding (testCaseCata)
import           Bead.Invariants (UnitTests(..))
#endif

-- Assignment aspect is a property of an assignment which
-- controls its visibility of start and end date, its controlls
-- over submission.
data Aspect
  = BallotBox -- Submission should not shown for the students only after the end of the dead line
  | Password String -- The assignment is password protected
  | ZippedSubmissions -- Submissions are zipped (i.e. binary files)
  deriving (Data, Eq, Show, Read, Ord, Typeable)

aspect
  ballot
  pwd
  zipped
  a = case a of
    BallotBox -> ballot
    Password p -> pwd p
    ZippedSubmissions -> zipped

data SubmissionType
  = TextSubmission
  | ZipSubmission
  deriving (Data, Eq, Show, Read, Ord, Typeable)

-- An assignment can have several aspects, which is a list represented
-- set. The reason here, is the set can not be converted to JSON representation
newtype Aspects = Aspects ([Aspect])
  deriving (Data, Eq, Read, Show, Typeable)

fromAspects :: (Set Aspect -> a) -> Aspects -> a
fromAspects f (Aspects x) = f $ Set.fromList x

toAspects :: (a -> Set Aspect) -> a -> Aspects
toAspects f x = Aspects (Set.toList $ f x)

-- | Assignment aspect set that does not contain any aspects
emptyAspects :: Aspects
emptyAspects = Aspects []

-- | Creates an AssignmentAspects from the given aspect list
-- suppossing that the list contains only one aspect at once
-- empty list represents an empty assignment aspect set.
fromList :: [Aspect] -> Aspects
fromList = toAspects Set.fromList

aspectsFromList = fromList

isPasswordAspect = aspect False (const True) False
isBallotBoxAspect = aspect True (const False) False
isZippedSubmissionsAspect = aspect False (const False) True

#ifdef TEST
assignmentAspectPredTests = group "assignmentAspectPred" $ do
  test "Password aspect predicate" $
    Equals True (isPasswordAspect (Password "pwd")) "Password aspect is not recognized"
  test "Ballow box aspect predicate" $
    Equals True (isBallotBoxAspect BallotBox) "Ballot box aspect is not recognized"
  test "Zipped submissions aspect predicate" $
    Equals True (isZippedSubmissionsAspect ZippedSubmissions) "Zipped submissions aspect is not recognized"
#endif

-- Returns True if the aspect set contains a password protected value
isPasswordProtected :: Aspects -> Bool
isPasswordProtected = fromAspects (not . Set.null . Set.filter isPasswordAspect)

#ifdef TEST
isPasswordProtectedTests = group "isPasswordProtected" $ do
  test "Empty aspect set"
       (Equals False (isPasswordProtected emptyAspects)
               "Empty set should not contain password")
  test "Non password aspects"
       (Equals False (isPasswordProtected (fromList [BallotBox]))
               "Ballot box set should not contain password")
  test "Password aspect"
       (Equals True (isPasswordProtected (fromList [Password ""]))
               "Password aspect should be found")
  test "Password aspect within more aspects"
       (Equals True (isPasswordProtected (fromList [Password "", BallotBox, ZippedSubmissions]))
               "Password aspect should be found")
#endif

-- Returns True if the aspect set contains a ballot box value
isBallotBox :: Aspects -> Bool
isBallotBox = fromAspects (not . Set.null . Set.filter isBallotBoxAspect)

#ifdef TEST
isBallotBoxTests = group "isBallotBox" $ do
  test "Empty aspect set"
       (Equals False (isBallotBox emptyAspects)
               "Empty set should not contain ballot box")
  test "Non password aspects"
       (Equals True (isBallotBox (fromList [BallotBox]))
               "Ballot box should be found")
  test "Password aspect"
       (Equals False (isBallotBox (fromList [Password ""]))
               "Password aspect should be rejected")
  test "Password aspect within more aspects"
       (Equals True (isBallotBox (fromList [Password "", BallotBox, ZippedSubmissions]))
               "BallotBox aspect should be found")
#endif

-- Returns True if the aspect set contains a "zipped submissions" value
isZippedSubmissions :: Aspects -> Bool
isZippedSubmissions = fromAspects (not . Set.null . Set.filter isZippedSubmissionsAspect)

setZippedSubmissions :: Aspects -> Aspects
setZippedSubmissions x@(Aspects as)
  | not (isZippedSubmissions x) = Aspects (ZippedSubmissions:as)
  | otherwise = x

clearZippedSubmissions :: Aspects -> Aspects
clearZippedSubmissions x@(Aspects as)
  | not (isZippedSubmissions x) = x
  | otherwise = Aspects $ filter (not . isZippedSubmissionsAspect) as

#ifdef TEST
isZippedSubmissionsTests = group "isZippedSubmissions" $ do
  test "Empty aspect set"
       (Equals False (isZippedSubmissions emptyAspects)
               "Empty set should not contain zipped submissions")
  test "Setting zipped submissions aspect"
       (Equals True (isZippedSubmissions $ setZippedSubmissions emptyAspects)
               "Zipped submissions aspect should be set")
  test "Clearing zipped submissions aspect"
       (Equals False (isZippedSubmissions $ clearZippedSubmissions $
          fromList [ZippedSubmissions])
               "Zipped submissions aspect should be cleared")
  test "Non password aspects"
       (Equals True (isZippedSubmissions (fromList [ZippedSubmissions]))
               "Zipped submissions should be found")
  test "Password aspect"
       (Equals False (isZippedSubmissions (fromList [Password ""]))
               "Password aspect should be rejected")
  test "Password aspect within more aspects"
       (Equals True (isZippedSubmissions (fromList [Password "", BallotBox, ZippedSubmissions]))
               "Zipped submissions aspect should be found")
#endif

-- Calculates True if the assignment aspects set contains at least one elements
-- That satisfies the preduicate
containsAspect :: (Aspect -> Bool) -> Aspects -> Bool
containsAspect pred = fromAspects (not . Set.null . Set.filter pred)

-- Returns the first password found in the assignment aspects set, if there
-- is no such password throws an error
getPassword :: Aspects -> String
getPassword = fromAspects $ \as ->
  case (Set.toList . Set.filter isPasswordAspect $ as) of
    [] -> error $ "getPassword: no password aspects was found"
    (pwd:_) -> aspect (error "getPassword: no password aspect was filtered in") id (error "getPassword: no password aspect was filtered in") pwd

-- | Set the assignments passwords in the assignment aspect set.
-- if the set already contains a password the password is replaced.
setPassword :: String -> Aspects -> Aspects
setPassword pwd = fromAspects updateOrSetPassword where
  updateOrSetPassword = Aspects . Set.toList . Set.insert (Password pwd) . Set.filter (not . isPasswordAspect)

#ifdef TEST
assignmentAspectsSetPasswordTests = group "setPassword" $ do
  test "Empty set" $ Equals
    (fromList [Password "password"])
    (setPassword "password" emptyAspects)
    "Password does not set in an empty aspect set."
  test "Replace only password" $ Equals
    (fromList [Password "new"])
    (setPassword "new" (fromList [Password "old"]))
    "Password is not replaced in a password empty set"
  test "Replace the password in a multiple set" $ Equals
    (fromList [ZippedSubmissions, BallotBox, Password "new"])
    (setPassword "new" (fromList [ZippedSubmissions, BallotBox, Password "old"]))
    "Password is not replaced in a non empty set"
#endif

-- | Assignment for the student
data Assignment = Assignment {
    name :: String
    -- ^ Name of the assignment
  , desc :: String
    -- ^ The text of the assignment itself
  , aspects :: Aspects
    -- ^ Aspects that modify the visibility of the assignment, or properties that
    -- modify the submission process
  , start :: UTCTime
    -- ^ The date since the submission can be sent for the assignment
  , end   :: UTCTime
    -- ^ The date until the submission is available for the assignment
  , evType :: EvConfig
    -- ^ The evaluatuin type for the assignment
  -- TODO: Number of maximum tries
  } deriving (Eq, Show)

-- | Template function for the assignment
assignmentCata f (Assignment name desc aspect start end evtype) =
  f name desc aspect start end evtype

-- | Template function for the assignment with flipped arguments
withAssignment a f = assignmentCata f a

assignmentAna name desc aspect start end evtype =
  Assignment <$> name <*> desc <*> aspect <*> start <*> end <*> evtype

-- | Produces True if the given time is between the start-end time of the assignment
isActive :: Assignment -> UTCTime -> Bool
isActive a t = and [start a <= t, t <= end a]

#ifdef TEST

assignmentTests =
  let a = Assignment {
          name = "name"
        , desc = "desc"
        , aspects = emptyAspects
        , start = read "2010-10-10 12:00:00 UTC"
        , end   = read "2010-11-10 12:00:00 UTC"
        , evType = percentageConfig 0.1
        }
      before  = read "2010-09-10 12:00:00 UTC"
      between = read "2010-10-20 12:00:00 UTC"
      after   = read "2010-12-10 12:00:00 UTC"
  in UnitTests [
    ("Time before active period", isFalse $ isActive a before)
  , ("Time in active period"    , isTrue  $ isActive a between)
  , ("Time after active period" , isFalse $ isActive a after)
  ]
  where
    isFalse = not
    isTrue  = id

asgTests = group "Bead.Domain.Entity.Assignment" $ do
  isPasswordProtectedTests
  isBallotBoxTests
  isZippedSubmissionsTests
  assignmentAspectPredTests
  assignmentAspectsSetPasswordTests
#endif
