module Bead.Persistence.SQL.JSON where

import           Text.JSON.Generic

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Entity.Assignment as Domain
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

encodeEvalConfig :: Domain.EvaluationConfig -> JSONText
encodeEvalConfig = encodeJSON

decodeEvalConfig :: JSONText -> Domain.EvaluationConfig
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

encodeEvaluationResult :: Domain.EvaluationResult -> JSONText
encodeEvaluationResult = encodeJSON

decodeEvaluationResult :: JSONText -> Domain.EvaluationResult
decodeEvaluationResult = decodeJSON

encodeCommentType :: Domain.CommentType -> JSONText
encodeCommentType = encodeJSON

decodeCommentType :: JSONText -> Domain.CommentType
decodeCommentType = decodeJSON

