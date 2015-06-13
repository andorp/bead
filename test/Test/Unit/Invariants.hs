{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Unit.Invariants (
    tests
  ) where

import           Test.Tasty.TestSet (group)

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
import qualified Bead.View.Content.All as CA (pageContentTest)
import           Bead.View.Content.Home.Page
import qualified Bead.View.DataBridge as DB (dataBridgeTests)
#ifdef EmailEnabled
import qualified Bead.View.EmailTemplate as E (runEmailTemplateTests)
#endif
import qualified Bead.View.Headers.AcceptLanguage as AL (acceptLanguageTests)
import qualified Bead.View.Pagelets as VP (linkTextTest)
import qualified Bead.View.RouteOf as R (routeOfTest)
import qualified Bead.View.Routing as R (routingTest)
import qualified Bead.View.Session as VS (uniqueSessionKeysTest)
import qualified Bead.View.TemplateAndComponentNames as TC (fieldNameTest)
#ifdef EmailEnabled
import qualified Bead.View.Validators as V (emailAddressTests)
#endif

tests = do
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
#ifdef EmailEnabled
  V.emailAddressTests
  E.runEmailTemplateTests
#endif
  PR.persistRelationsTests
  DB.dataBridgeTests
  F.feedbackTests
  CP.parseTests
  AL.acceptLanguageTests
