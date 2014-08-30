module Bead.Persistence.SQL.JSON where

import           Text.JSON.Generic

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Shared.Evaluation as Domain
import           Bead.Persistence.SQL.Entities (JSONText)

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

encodeTestCaseType :: Domain.TestCaseType -> JSONText
encodeTestCaseType = encodeJSON

decodeTestCaseType :: JSONText -> Domain.TestCaseType
decodeTestCaseType = decodeJSON

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
