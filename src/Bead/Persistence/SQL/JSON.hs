{-# LANGUAGE CPP #-}
module Bead.Persistence.SQL.JSON where

import           Text.JSON.Generic

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Shared.Evaluation as Domain
import           Bead.Persistence.SQL.Entities (JSONText)

#ifdef TEST
import           Test.Tasty.Arbitrary
import           Test.Tasty.TestSet
#endif

-- * JSON encoding, decoding

encodeRole :: Domain.Role -> JSONText
encodeRole = encodeJSON

decodeRole :: JSONText -> Domain.Role
decodeRole = decodeJSON

encodeTimeZone :: Domain.TimeZoneName -> JSONText
encodeTimeZone = encodeJSON

decodeTimeZone :: JSONText -> Domain.TimeZoneName
decodeTimeZone = decodeJSON

encodeEvalConfig :: Domain.EvConfig -> JSONText
encodeEvalConfig = encodeJSON

decodeEvalConfig :: JSONText -> Domain.EvConfig
decodeEvalConfig = decodeJSON

encodeTestScriptType :: Domain.TestScriptType -> JSONText
encodeTestScriptType = encodeJSON

decodeTestScriptType :: JSONText -> Domain.TestScriptType
decodeTestScriptType = decodeJSON

encodeAssignmentType :: Domain.Aspects -> JSONText
encodeAssignmentType = encodeJSON

decodeAssignmentType :: JSONText -> Domain.Aspects
decodeAssignmentType = decodeJSON

encodeEvaluationResult :: Domain.EvResult -> JSONText
encodeEvaluationResult = encodeJSON

decodeEvaluationResult :: JSONText -> Domain.EvResult
decodeEvaluationResult = decodeJSON

encodeCommentType :: Domain.CommentType -> JSONText
encodeCommentType = encodeJSON

decodeCommentType :: JSONText -> Domain.CommentType
decodeCommentType = decodeJSON

encodeFeedbackInfo :: Domain.FeedbackInfo -> JSONText
encodeFeedbackInfo = encodeJSON

decodeFeedbackInfo :: JSONText -> Domain.FeedbackInfo
decodeFeedbackInfo = decodeJSON

encodeScore :: Domain.Score -> JSONText
encodeScore = encodeJSON

decodeScore :: JSONText -> Domain.Score
decodeScore = decodeJSON

#ifdef TEST
persistJSONConvertTests = group "Persistence JSON converters" $ do
  idempotent "Score" encodeScore decodeScore
  idempotent "Role" encodeRole decodeRole
  idempotent "TimeZone" encodeTimeZone decodeTimeZone
  idempotent "EvalConfig" encodeEvalConfig decodeEvalConfig
  idempotent "TestScriptType" encodeTestScriptType decodeTestScriptType
  idempotent "AssignmentType" encodeAssignmentType decodeAssignmentType
  idempotent "EvaluationResult" encodeEvaluationResult decodeEvaluationResult
  idempotent "CommentType" encodeCommentType decodeCommentType
  idempotent "FeedbackInfo " encodeFeedbackInfo decodeFeedbackInfo
  where
    idempotent :: (Arbitrary a, Eq a, Show a)
               => TestName -> (a -> b) -> (b -> a) -> TestSet ()
    idempotent name encode decode =
      assertProperty
        name
        (\x -> x == (decode $ encode x))
        arbitrary
        (concat ["Encode decode of ", name, " is not idempotent"])
#endif
