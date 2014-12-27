module Bead.View.RequestParams where

import Control.Monad (join)
import Data.String (IsString(..))
import Bead.Domain.Entities (Username(..))
import Bead.Domain.Relationships
import Bead.View.Dictionary (Language, languageCata)
import Bead.View.Fay.HookIds
import Bead.View.TemplateAndComponentNames

-- Request Parameter Name Constants

assignmentKeyParamName :: IsString s => s
assignmentKeyParamName = fromString $ fieldName assignmentKeyField

submissionKeyParamName :: IsString s => s
submissionKeyParamName = fromString $ fieldName submissionKeyField

evaluationKeyParamName :: IsString s => s
evaluationKeyParamName = fromString $ fieldName evaluationKeyField

languageParamName :: IsString s => s
languageParamName = fromString $ fieldName changeLanguageField

courseKeyParamName :: IsString s => s
courseKeyParamName = fromString $ fieldName courseKeyField

groupKeyParamName :: IsString s => s
groupKeyParamName = fromString $ fieldName groupKeyField

testScriptKeyParamName :: IsString s => s
testScriptKeyParamName = fromString $ fieldName testScriptKeyField

-- Request Param is a Pair of Strings, which
-- are key and value representing a parameter in
-- the GET or POST http request
newtype ReqParam = ReqParam (String,String)

-- Produces a string representing the key value pair
-- E.g: ReqParam ("name", "rika") = "name=rika"
queryStringParam :: ReqParam -> String
queryStringParam (ReqParam (k,v)) = join [k, "=", v]

-- Values that can be converted into a request param,
-- only the value of the param is calculated
class ReqParamValue p where
  paramValue :: (IsString s) => p -> s

-- Values that can be converted into request param,
-- the name and the value is also calculated
class (ReqParamValue r) => RequestParam r where
  requestParam :: r -> ReqParam


instance ReqParamValue AssignmentKey where
  paramValue (AssignmentKey a) = fromString a

instance RequestParam AssignmentKey where
  requestParam a = ReqParam (assignmentKeyParamName, paramValue a)

instance ReqParamValue SubmissionKey where
  paramValue (SubmissionKey s) = fromString s

instance RequestParam SubmissionKey where
  requestParam s = ReqParam (submissionKeyParamName, paramValue s)

instance ReqParamValue GroupKey where
  paramValue (GroupKey g) = fromString g

instance RequestParam GroupKey where
  requestParam g = ReqParam (groupKeyParamName, paramValue g)

instance ReqParamValue TestScriptKey where
  paramValue (TestScriptKey t) = fromString t

instance RequestParam TestScriptKey where
  requestParam t = ReqParam (testScriptKeyParamName, paramValue t)

instance ReqParamValue CourseKey where
  paramValue (CourseKey c) = fromString c

instance RequestParam CourseKey where
  requestParam g = ReqParam (courseKeyParamName, paramValue g)

instance ReqParamValue EvaluationKey where
  paramValue (EvaluationKey e) = fromString e

instance RequestParam EvaluationKey where
  requestParam e = ReqParam (evaluationKeyParamName, paramValue e)

instance ReqParamValue Username where
  paramValue (Username u) = fromString u

instance RequestParam Username where
  requestParam u = ReqParam (fieldName usernameField, paramValue u)

instance ReqParamValue Language where
  paramValue = languageCata fromString

instance RequestParam Language where
  requestParam l = ReqParam (languageParamName, paramValue l)
