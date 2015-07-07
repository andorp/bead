{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Entity.Assignment (
    Aspect(..)
  , aspect
  , SubmissionType(..)
  , submissionType
  , Aspects
  , createAspects
  , fromAspects
  , toAspects
  , emptyAspects
  , fromList
  , aspectsFromList
  , isPasswordAspect
  , isBallotBoxAspect
  , isIsolatedAspect
  , getPassword
  , setPassword
  , isNoOfTries
  , getNoOfTries
  , setNoOfTries
  , noOfTries
  , clearNoOfTries

  , isPasswordProtected
  , isBallotBox
  , isZippedSubmissions
  , isIsolated
  , setZippedSubmissions
  , clearZippedSubmissions
  , aspectsToSubmissionType

  , Assignment(..)
  , assignmentCata
  , withAssignment
  , assignmentAna
  , isActive

#ifdef TEST
  , asgTests
#endif
  ) where

import           Control.Applicative
import           Data.Data
import           Data.List (find)
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime(..))

import           Bead.Domain.Shared.Evaluation

#ifdef TEST
import           Test.Tasty.Arbitrary
import           Test.Tasty.RandomData
import           Test.Tasty.TestSet
import           Test.QuickCheck.Gen
#endif

-- Assignment aspect is a property of an assignment which
-- controls its visibility of start and end date, its controlls
-- over submission.
data Aspect
  = BallotBox -- Submission should not shown for the students only after the end of the dead line
  | Password String -- The assignment is password protected
  | ZippedSubmissions -- Submissions are zipped (i.e. binary files)
  | Isolated -- When an assignment is isolated the others related are not visible for the user
  | NoOfTries Int -- It limits the number of the submissions that can be sent for the assignment
  deriving (Data, Eq, Show, Read, Ord, Typeable)

aspect
  ballot
  pwd
  zipped
  isolated
  noOfTries
  a = case a of
    BallotBox -> ballot
    Password p -> pwd p
    ZippedSubmissions -> zipped
    Isolated -> isolated
    NoOfTries n -> noOfTries n

#ifdef TEST
instance RandomData Aspect where
  positive = oneof
    [ elements [BallotBox, ZippedSubmissions, Isolated]
    , Password <$> listOf1 alphaNum
    , NoOfTries <$> choose (1, 100)
    ]
  negative = oneof
    [ return (Password "")
    , NoOfTries <$> choose ((-2), 0)
    ]

instance Arbitrary Aspect where
  arbitrary = positive
#endif

-- Submission Type of the assignment, this information
-- will be stored as an aspect
data SubmissionType
  = TextSubmission
  | ZipSubmission
  deriving (Data, Eq, Show, Read, Ord, Typeable)

submissionType
  text
  zip
  s = case s of
    TextSubmission -> text
    ZipSubmission -> zip

#ifdef TEST
instance Arbitrary SubmissionType where
  arbitrary = elements [TextSubmission, ZipSubmission]
#endif

-- An assignment can have several aspects, which is a list represented
-- set. The reason here, is the set can not be converted to JSON representation
newtype Aspects = Aspects ([Aspect])
  deriving (Data, Eq, Read, Show, Typeable)

-- Creates Aspects from the given list of aspect keeping only the first occurrence
-- of every aspect, supposing that the list is not infinite.
createAspects :: [Aspect] -> Aspects
createAspects as = Aspects . catMaybes $ map (find' as) preds
  where
    find' = flip find
    preds = [ isPasswordAspect, isBallotBoxAspect, isZippedSubmissionsAspect, isIsolatedAspect, isNoOfTriesAspect ]

-- If the aspects is not recognized by the predicate it returns the given value
-- otherwise extract the information and applies the transformation to it.
aspectsMap :: (Aspects -> Bool) -> (Aspects -> a) -> b -> (a -> b) -> Aspects -> b
aspectsMap pred extract def trans a = if pred a then trans (extract a) else def

#ifdef TEST
instance RandomData Aspects where
  positive = createAspects <$> listOf positive
  negative = createAspects <$> listOf negative

instance Arbitrary Aspects where
  arbitrary = positive
#endif

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

isPasswordAspect = aspect False (const True) False False (const False)
isBallotBoxAspect = aspect True (const False) False False (const False)
isZippedSubmissionsAspect = aspect False (const False) True False (const False)
isIsolatedAspect = aspect False (const False) False True (const False)
isNoOfTriesAspect = aspect False (const False) False False (const True)

#ifdef TEST
assignmentAspectPredTests = group "assignmentAspectPred" $ do
  assertEquals "Password aspect predicate" True (isPasswordAspect (Password "pwd")) "Password aspect is not recognized"
  assertEquals "Ballow box aspect predicate" True (isBallotBoxAspect BallotBox) "Ballot box aspect is not recognized"
  assertEquals "Zipped submissions aspect predicate" True (isZippedSubmissionsAspect ZippedSubmissions) "Zipped submissions aspect is not recognized"
  assertEquals "Isolated assignment" True (isIsolatedAspect Isolated) "Isolated aspect is not recognized"
  assertEquals "No of tries" True (isNoOfTriesAspect (NoOfTries 4)) "No of tries is not recognized"
#endif

-- Returns True if the aspect set contains a password protected value
isPasswordProtected :: Aspects -> Bool
isPasswordProtected = fromAspects (not . Set.null . Set.filter isPasswordAspect)

#ifdef TEST
isPasswordProtectedTests = group "isPasswordProtected" $ do
  assertEquals "Empty aspect set" False (isPasswordProtected emptyAspects) "Empty set should not contain password"
  assertEquals "Non password aspects" False (isPasswordProtected (fromList [BallotBox])) "Ballot box set should not contain password"
  assertEquals "Password aspect" True (isPasswordProtected (fromList [Password ""])) "Password aspect should be found"
  assertEquals "Password aspect within more aspects" True (isPasswordProtected (fromList [Password "", BallotBox, ZippedSubmissions, Isolated]))
               "Password aspect should be found"
  assertProperty "Password protected positive random tests (password is given)"
                 isPasswordProtected
                 ((createAspects . (Password "a":)) <$> listOf positive)
                 "Password protected is not recognized"
  assertProperty "Password protected positive random tests (password is not given)"
                 (not . isPasswordProtected)
                 ((createAspects . (filter (not . isPasswordAspect))) <$> listOf positive)
                 "Non password protected is recognized"
#endif

-- Returns True if the aspect set contains a ballot box value
isBallotBox :: Aspects -> Bool
isBallotBox = fromAspects (not . Set.null . Set.filter isBallotBoxAspect)

#ifdef TEST
isBallotBoxTests = group "isBallotBox" $ do
  assertEquals "Empty aspect set" False (isBallotBox emptyAspects) "Empty set should not contain ballot box"
  assertEquals "Non password aspects" True (isBallotBox (fromList [BallotBox])) "Ballot box should be found"
  assertEquals "Password aspect" False (isBallotBox (fromList [Password ""])) "Password aspect should be rejected"
  assertEquals "Password aspect within more aspects" True (isBallotBox (fromList [Password "", BallotBox, ZippedSubmissions, Isolated]))
               "BallotBox aspect should be found"
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
isZippedSubmissionsTests = group "isZippedSubmissions" $
  eqPartitions isZippedSubmissions
    [ Partition "Empty aspect set" emptyAspects False "Empty set should not contain zipped submissions"
    , Partition "Setting zipped submissions aspect"
                (setZippedSubmissions emptyAspects)
                True "Zipped submissions aspect should be set"
    , Partition "Clearing zipped submissions aspect"
                (clearZippedSubmissions $ fromList [ZippedSubmissions])
                False "Zipped submissions aspect should be cleared"
    , Partition "Non password aspects" (fromList [ZippedSubmissions]) True "Zipped submissions should be found"
    , Partition "Password aspect" (fromList [Password ""]) False "Password aspect should be rejected"
    , Partition "Password aspect within more aspects"
                (fromList [Password "", BallotBox, ZippedSubmissions])
                True "Zipped submissions aspect should be found"
    ]
#endif

-- Returns True if the aspect set contains a "isolated" value
isIsolated :: Aspects -> Bool
isIsolated = fromAspects (not . Set.null . Set.filter isIsolatedAspect)

-- Extract the submission type from the aspects set
aspectsToSubmissionType :: Aspects -> SubmissionType
aspectsToSubmissionType x = if isZippedSubmissions x then ZipSubmission else TextSubmission

#ifdef TEST
aspectsToSubmissionTypeTests = group "aspectsToSubmissionType" $
  eqPartitions aspectsToSubmissionType
    [ Partition "Zipped Submission" (fromList [ZippedSubmissions]) ZipSubmission "Zipped submission should be found"
    , Partition "Emtpy aspect set" emptyAspects TextSubmission "Empty set should not contain zipped submissions"
    , Partition "Password aspect" (fromList [Password ""]) TextSubmission "Password aspect should be rejected"
    ]
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
    []      -> err
    (pwd:_) -> aspect err id err err err pwd
  where
    err = error "getPassword: no password aspect was filtered in"

-- | Set the assignments passwords in the assignment aspect set.
-- if the set already contains a password the password is replaced.
setPassword :: String -> Aspects -> Aspects
setPassword pwd = fromAspects updateOrSetPassword where
  updateOrSetPassword = Aspects . Set.toList . Set.insert (Password pwd) . Set.filter (not . isPasswordAspect)

#ifdef TEST
assignmentAspectsSetPasswordTests = group "setPassword" $ do
  assertEquals "Empty set"
               (fromList [Password "password"])
               (setPassword "password" emptyAspects)
               "Password does not set in an empty aspect set."
  assertEquals "Replace only password"
               (fromList [Password "new"])
               (setPassword "new" (fromList [Password "old"]))
               "Password is not replaced in a password empty set"
  assertEquals "Replace the password in a multiple set"
               (fromList [ZippedSubmissions, BallotBox, Password "new", Isolated])
               (setPassword "new" (fromList [ZippedSubmissions, BallotBox, Password "old", Isolated]))
               "Password is not replaced in a non empty set"
#endif

-- Returns True if the no of submissions flag is active
isNoOfTries :: Aspects -> Bool
isNoOfTries = containsAspect isNoOfTriesAspect

-- Returns the first values no of tries found in the assignment aspects set, if there
-- is no such value throws an error
getNoOfTries :: Aspects -> Int
getNoOfTries = fromAspects $ \as ->
  case (Set.toList . Set.filter isNoOfTriesAspect $ as) of
    []     -> err
    (no:_) -> aspect err err err err id no
  where
    err = error "getNoOfTries: no no of tries aspect was filtered in"

-- Returns the 'x' value if the aspects does not contain the noOfTries aspect
-- otherwise applies the function to it.
noOfTries :: a -> (Int -> a) -> Aspects -> a
noOfTries = aspectsMap isNoOfTries getNoOfTries

-- | Set the assignments passwords in the assignment aspect set.
-- if the set already contains a password the password is replaced.
setNoOfTries :: Int -> Aspects -> Aspects
setNoOfTries no = fromAspects updateOrSet where
  updateOrSet = Aspects . Set.toList . Set.insert (NoOfTries no) . Set.filter (not . isNoOfTriesAspect)

-- | Clears the number of tries aspect from the aspects
clearNoOfTries :: Aspects -> Aspects
clearNoOfTries = fromAspects clear where
  clear = Aspects . Set.toList . Set.filter (not . isNoOfTriesAspect)

#ifdef TEST
noOfTriesTests = do
  assertProperty "noOfTries positive tests (no of tries is given)"
                 isNoOfTries
                 (createAspects . ((NoOfTries 1):) <$> listOf positive)
                 "No of tries is not recognized"
  assertProperty "noOfTries positive tests (no of tries is not given)"
                 (not . isNoOfTries)
                 (createAspects . (filter (not . isNoOfTriesAspect)) <$> listOf positive)
                 "No of tries is recognized"
  assertProperty "noOfTests: get (set n aspects) == n"
                 id
                 (do a <- createAspects <$> listOf positive
                     n <- choose (1,100)
                     return $ getNoOfTries (setNoOfTries n a) == n)
                 "Set/Get property is broken"
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
instance Arbitrary Assignment where
  arbitrary =
    Assignment
    <$> listOf1 alphaNum
    <*> listOf1 alphaNum
    <*> arbitrary
    <*> (pure (read "2014-12-12 18:56:29.363547 UTC"))
    <*> (pure (read "2015-12-12 18:56:29.363547 UTC"))
    <*> (percentageConfig <$> choose (0.0, 1.0))
#endif

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
  in eqPartitions isActive' [
    Partition "Time before active period" (a, before)  False ""
  , Partition "Time in active period"     (a, between) True  ""
  , Partition "Time after active period"  (a, after)   False ""
  ]
  where
    isActive' = uncurry isActive
    isFalse = not
    isTrue  = id

asgTests = group "Bead.Domain.Entity.Assignment" $ do
  isPasswordProtectedTests
  isBallotBoxTests
  isZippedSubmissionsTests
  aspectsToSubmissionTypeTests
  assignmentAspectPredTests
  assignmentAspectsSetPasswordTests
  noOfTriesTests
  assignmentTests
#endif
