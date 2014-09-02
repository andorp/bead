{-# LANGUAGE TypeFamilies #-}
module Bead.Persistence.SQL.Class where

import qualified Data.Text as Text
import           Database.Persist.Sqlite

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import           Bead.Persistence.SQL.Entities
import           Bead.Persistence.SQL.JSON

-- * DomainKey and DomainValue definitions

-- | Represents conversational functions between a domain
-- key and an entity key
class DomainKey k where
  type EntityForKey k :: * -> *
  toDomainKey   :: KeyBackend backend entity -> k
  fromDomainKey :: k -> KeyBackend backend entity

-- | Creates a key value from the entity key supposing that the entity key
-- is an integer
entityToDomainKey domainKey name = persistInt . unKey where
  persistInt (PersistInt64 k) = domainKey $ show k
  persistInt k = persistError name $ concat ["invalid entity key ", show k]

domainKeyToEntityKey fromDomain key = Key . PersistInt64 . read $ fromDomain key

-- | Converts a domain key to an entity key
toEntityKey :: (DomainKey k, entity ~ EntityForKey k) => k -> KeyBackend backend (entity backend)
toEntityKey = fromDomainKey

-- | Converts an entity key to a domain key
fromEntityKey :: (DomainKey k, entity ~ EntityForKey k) => KeyBackend backend (entity backend) -> k
fromEntityKey = toDomainKey

-- | Rerpesnts conversional functions between a domain
-- value and persistent value
class DomainValue v where
  type EntityValue v :: * -> *
  toDomainValue      :: (EntityValue v) backend -> v
  fromDomainValue    :: v -> (EntityValue v) backend

-- | Converts an entity value to a domain value
fromEntityValue :: (DomainValue v, e ~ EntityValue v) => e backend -> v
fromEntityValue = toDomainValue

-- | Converts a domain value to an entity value
toEntityValue :: (DomainValue v, e ~ EntityValue v) => v -> e backend
toEntityValue = fromDomainValue

-- * Instances

instance DomainValue Domain.User where
  type EntityValue Domain.User = UserGeneric

  toDomainValue ent = Domain.User
    (decodeRole $ userRole ent)
    (Domain.Username . Text.unpack $ userUsername ent)
    (Domain.Email . Text.unpack $ userEmail ent)
    (Text.unpack $ userName ent)
    (decodeTimeZone $ userTimeZone ent)
    (Domain.Language . Text.unpack $ userLanguage ent)

  fromDomainValue = Domain.userCata $ \role username email name timezone language -> User
    (encodeRole role)
    (Domain.usernameCata Text.pack username)
    (Domain.emailCata Text.pack email)
    (Text.pack name)
    (encodeTimeZone timezone)
    (Domain.languageCata Text.pack language)


instance DomainKey Domain.UserRegKey where
  type EntityForKey Domain.UserRegKey = UserRegistrationGeneric
  fromDomainKey = domainKeyToEntityKey $ \(Domain.UserRegKey k) -> k
  toDomainKey   = entityToDomainKey Domain.UserRegKey "entityKeyToUserRegKey"


instance DomainValue Domain.UserRegistration where
  type EntityValue Domain.UserRegistration = UserRegistrationGeneric

  fromDomainValue = Domain.userRegistration $ \username email name token timeout -> UserRegistration
    (Text.pack username)
    (Text.pack email)
    (Text.pack name)
    (Text.pack token)
    timeout

  toDomainValue ent = Domain.UserRegistration
    (Text.unpack $ userRegistrationUsername ent)
    (Text.unpack $ userRegistrationEmail ent)
    (Text.unpack $ userRegistrationName ent)
    (Text.unpack $ userRegistrationToken ent)
    (userRegistrationTimeout ent)


instance DomainKey Domain.CourseKey where
  type EntityForKey Domain.CourseKey = CourseGeneric
  fromDomainKey = domainKeyToEntityKey $ \(Domain.CourseKey k) -> k
  toDomainKey   = entityToDomainKey Domain.CourseKey "entityKeyToCourseKey"

instance DomainValue Domain.Course where
  type EntityValue Domain.Course = CourseGeneric

  toDomainValue ent = Domain.Course
    (Text.unpack $ courseName ent)
    (Text.unpack $ courseDescription ent)
    (decodeTestScriptType $ courseTestScriptType ent)

  fromDomainValue = Domain.courseCata encodeTestScriptType
    $ \name description testScriptType ->
         Course (Text.pack name)
                (Text.pack description)
                testScriptType

instance DomainKey Domain.GroupKey where
  type EntityForKey Domain.GroupKey = GroupGeneric
  fromDomainKey = domainKeyToEntityKey $ \(Domain.GroupKey k) -> k
  toDomainKey   = entityToDomainKey Domain.GroupKey "entityKeyToGroupKey"

instance DomainValue Domain.Group where
  type EntityValue Domain.Group = GroupGeneric

  toDomainValue ent = Domain.Group
    (Text.unpack $ groupName ent)
    (Text.unpack $ groupDescription ent)

  fromDomainValue = Domain.groupCata
        $ \name description ->
            Group (Text.pack name)
                  (Text.pack description)

instance DomainKey Domain.TestScriptKey where
  type EntityForKey Domain.TestScriptKey = TestScriptGeneric
  fromDomainKey = domainKeyToEntityKey $ \(Domain.TestScriptKey k) -> k
  toDomainKey   = entityToDomainKey Domain.TestScriptKey "entityKeyToTestScriptKey"

instance DomainValue Domain.TestScript where
  type EntityValue Domain.TestScript = TestScriptGeneric

  toDomainValue ent = Domain.TestScript
    (Text.unpack $ testScriptName ent)
    (Text.unpack $ testScriptDescription ent)
    (Text.unpack $ testScriptNotes ent)
    (Text.unpack $ testScriptScript ent)
    (decodeTestScriptType $ testScriptTestScriptType ent)

  fromDomainValue = Domain.testScriptCata encodeTestScriptType
    $ \name desc notes script type_ ->
        TestScript (Text.pack name)
                   (Text.pack desc)
                   (Text.pack notes)
                   (Text.pack script)
                   (type_)


instance DomainKey Domain.TestCaseKey where
  type EntityForKey Domain.TestCaseKey = TestCaseGeneric
  fromDomainKey = domainKeyToEntityKey $ \(Domain.TestCaseKey k) -> k
  toDomainKey   = entityToDomainKey Domain.TestCaseKey "entityKeyToTestCaseKey"

instance DomainValue Domain.TestCase where
  type EntityValue Domain.TestCase = TestCaseGeneric

  toDomainValue ent = Domain.TestCase
    (Text.unpack $ testCaseName ent)
    (Text.unpack $ testCaseDescription ent)
    (case (testCaseSimpleValue ent, testCaseZippedValue ent) of
       (Just simple, Nothing) -> Domain.SimpleTestCase $ Text.unpack simple
       (Nothing, Just zipped) -> Domain.ZippedTestCase zipped
       (Just _, Just _)   -> error "toDomainTestCase: simple and zipped value were given"
       (Nothing, Nothing) -> error "toDomainTestCase: simple or zipped value were given")
    (Text.unpack $ testCaseInfo ent)

  fromDomainValue = Domain.testCaseCata id -- encodeTestCaseType
    $ \name desc value {-type_-} info ->
        let (simple, zipped) = Domain.withTestCaseValue
                                 value
                                 (fstNothing Text.pack)
                                 (nothingSnd id)
        in TestCase (Text.pack name)
                    (Text.pack desc)
                    simple
                    zipped
                    (Text.pack info)

fstNothing f s = (Just $ f s, Nothing)
nothingSnd f s = (Nothing, Just $ f s)

instance DomainKey Domain.AssignmentKey where
  type EntityForKey Domain.AssignmentKey = AssignmentGeneric
  fromDomainKey = domainKeyToEntityKey $ \(Domain.AssignmentKey k) -> k
  toDomainKey   = entityToDomainKey Domain.AssignmentKey "entityKeyToAssignmentKey"


instance DomainKey Domain.SubmissionKey where
  type EntityForKey Domain.SubmissionKey = SubmissionGeneric
  fromDomainKey = domainKeyToEntityKey $ \(Domain.SubmissionKey k) -> k
  toDomainKey   = entityToDomainKey Domain.SubmissionKey "entityKeyToSubmissionKey"

instance DomainValue Domain.Submission where
  type EntityValue Domain.Submission = SubmissionGeneric

  fromDomainValue = Domain.submissionCata $ \submission postDate ->
    let (simple, zipped) = Domain.submissionValue
                             (fstNothing Text.pack)
                             (nothingSnd id)
                             submission
    in Submission simple
                  zipped
                  postDate

  toDomainValue ent =
    Domain.Submission
      (case (submissionSimple ent, submissionZipped ent) of
         (Just simple, Nothing) -> Domain.SimpleSubmission $ Text.unpack simple
         (Nothing, Just zipped) -> Domain.ZippedSubmission zipped
         (Nothing, Nothing) -> error "toDomainSubmission: no simple or zipped solution was given."
         (Just _, Just _)   -> error "toDomainSubmission: simple and zipped solution were given.")
      (submissionPostDate ent)

instance DomainKey Domain.EvaluationKey where
  type EntityForKey Domain.EvaluationKey = EvaluationGeneric
  fromDomainKey = domainKeyToEntityKey $ \(Domain.EvaluationKey k) -> k
  toDomainKey   = entityToDomainKey Domain.EvaluationKey "entityKeyToEvaluationKey"

instance DomainValue Domain.Evaluation where
  type EntityValue Domain.Evaluation = EvaluationGeneric

  fromDomainValue = Domain.evaluationCata $ \result written ->
    Evaluation (encodeEvaluationResult result)
               (Text.pack written)

  toDomainValue ent =
    Domain.Evaluation
      (decodeEvaluationResult $ evaluationResult ent)
      (Text.unpack $ evaluationWritten ent)

instance DomainKey Domain.CommentKey where
  type EntityForKey Domain.CommentKey = CommentGeneric
  fromDomainKey = domainKeyToEntityKey $ \(Domain.CommentKey k) -> k
  toDomainKey   = entityToDomainKey Domain.CommentKey "entityKeyToCommentKey"

instance DomainValue Domain.Comment where
  type EntityValue Domain.Comment = CommentGeneric

  fromDomainValue = Domain.commentCata $ \cmt author date type_ ->
    Comment (Text.pack cmt)
            (Text.pack author)
            date
            (encodeCommentType type_)

  toDomainValue ent =
    Domain.Comment
      (Text.unpack $ commentText ent)
      (Text.unpack $ commentAuthor ent)
      (commentDate ent)
      (decodeCommentType $ commentType ent)

instance DomainKey Domain.FeedbackKey where
  type EntityForKey Domain.FeedbackKey = FeedbackGeneric
  fromDomainKey = domainKeyToEntityKey $ \(Domain.FeedbackKey k) -> k
  toDomainKey   = entityToDomainKey Domain.FeedbackKey "entityToFeedbackKey"

instance DomainValue Domain.Feedback where
  type EntityValue Domain.Feedback = FeedbackGeneric

  fromDomainValue = Domain.feedback encodeFeedbackInfo Feedback

  toDomainValue ent =
    Domain.Feedback
      (decodeFeedbackInfo $ feedbackInfo ent)
      (feedbackDate ent)

instance DomainKey Domain.AssessmentKey where
  type EntityForKey Domain.AssessmentKey = AssessmentGeneric
  fromDomainKey = domainKeyToEntityKey $ \(Domain.AssessmentKey k) -> k
  toDomainKey   = entityToDomainKey Domain.AssessmentKey "entityToAssessmentKey"

instance DomainValue Domain.Assessment where
  type EntityValue Domain.Assessment = AssessmentGeneric

  fromDomainValue = Domain.assessment $
    \desc cfg -> Assessment
      (Text.pack desc)
      (encodeEvalConfig cfg)

  toDomainValue ent = Domain.Assessment
    (Text.unpack $ assessmentDescription ent)
    (decodeEvalConfig $ assessmentEvalConfig ent)

instance DomainKey Domain.ScoreKey where
  type EntityForKey Domain.ScoreKey = ScoreGeneric
  fromDomainKey = domainKeyToEntityKey $ \(Domain.ScoreKey k) -> k
  toDomainKey   = entityToDomainKey Domain.ScoreKey "entityToScoreKey"

instance DomainValue Domain.Score where
  type EntityValue Domain.Score = ScoreGeneric
  fromDomainValue _s = Score "score"
  toDomainValue _ent = Domain.Score
