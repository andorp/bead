{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Unit.Invariants (
    tests
  , tastyTests
  ) where

import           Control.Monad (join)

import           Test.Framework (Test(..), testGroup)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (Test(..), Assertion (..))
import           Test.Tasty.TestSet (group)
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Monadic

import           Bead.Config (initTaskAssertions)
import qualified Bead.Config.Parser as CP (parseTests)
import qualified Bead.Controller.Pages as P (pageDescTest)
import qualified Bead.Daemon.Logout as LD (logoutQueueTests)
import qualified Bead.Domain.Entities as E
import qualified Bead.Domain.Entity.Assignment as A
import qualified Bead.Domain.Entity.Feedback as F
import           Bead.Domain.Relationships (relationshipTests)
import qualified Bead.Domain.RolePermission as RP (permissionTest)
import qualified Bead.Persistence.NoSQLDirFile as L (noSqlDirTests)
import qualified Bead.Persistence.Persist as P (persistTests)
import qualified Bead.Persistence.Relations as PR (persistRelationsTests)
import           Bead.Invariants
import qualified Bead.View.Content.All as CA (pageContentTest)
import           Bead.View.Content.Home.Page
import qualified Bead.View.DataBridge as DB (dataBridgeTests)
#ifdef EmailEnabled
import qualified Bead.View.EmailTemplate as E (unitTests)
#endif
import qualified Bead.View.Headers.AcceptLanguage as AL (acceptLanguageTests)
import qualified Bead.View.Pagelets as VP (linkTextTest)
import qualified Bead.View.RouteOf as R (routeOfTest)
import qualified Bead.View.Routing as R (routingTest)
import qualified Bead.View.Session as VS (uniqueSessionKeysTest)
import qualified Bead.View.TemplateAndComponentNames as TC (fieldNameTest)
#ifdef EmailEnabled
import qualified Bead.View.Validators as V (assertEmailAddress)
#endif


unitTestGroup :: String -> UnitTests -> Test
unitTestGroup name (UnitTests ts) = testGroup name
  $ map (\(desc, testcase) -> testCase desc (assertBool desc testcase)) ts

ioUnitTestGroup :: String -> UnitTestsM IO -> Test
ioUnitTestGroup name (UnitTestsM ts) = testGroup name
  $ map (\(desc, testcase) -> testCase desc (testcase >>= assertBool desc)) ts

invariantsGroup :: (Show i, Arbitrary i) => String -> Invariants i -> Test
invariantsGroup name (Invariants is) = testGroup name
  $ map (\(desc, prop) -> testProperty desc prop) is

invariantsIO2Group
  :: (Show a, Show b, Arbitrary a, Arbitrary b)
  => String -> InvariantsM2 IO a b -> Test
invariantsIO2Group name (InvariantsM2 is) = testGroup name
  $ map (\(desc, prop) -> testProperty desc (\a b -> monadicIO (run (prop a b)))) is

assertionTestGroup :: (Show a, Eq a) => String -> [Assertion a] -> Test
assertionTestGroup name as = testGroup name
  $ map (assertionMap createTestCase createOracleTestCase) as
  where
    createTestCase n f e =
      testCase n $ assertBool (join ["Found: ", show f, " expected: ", show e]) (f == e)

    createOracleTestCase n f o =
      testCase n $
        assertBool (join ["Found: ", show f, " The oracle does not accept the result."]) (o f)

-- * Unit tests

tests = [
#ifdef EmailEnabled
    ioUnitTestGroup "Email template tests" E.unitTests
  , assertionTestGroup "Email address" V.assertEmailAddress
#endif
 ]

tastyTests = do
  group "Page description" P.pageDescTest
  group "Route of" R.routeOfTest
  group "Routing" R.routingTest
  group "Page content handler " CA.pageContentTest
  group "Link text" VP.linkTextTest
  group "Logout daemon" LD.logoutQueueTests
  group "Permissions" RP.permissionTest
  group "NoSQLDir" L.noSqlDirTests
  group "Field name" TC.fieldNameTest
  group "Entity" E.entityTests
  relationshipTests
  group "Unique session keys" VS.uniqueSessionKeysTest
  group "Assignment" A.asgTests
  group "Homepage binary results" sumBinaryResultTests
  group "Homepage percentage results" sumPercentageResultTests
  group "Command line and configuration" initTaskAssertions
  P.persistTests
  PR.persistRelationsTests
  DB.dataBridgeTests
  F.feedbackTests
  CP.parseTests
  AL.acceptLanguageTests
