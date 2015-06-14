{-# LANGUAGE CPP #-}
--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE FlexibleInstances #-}
module Test.Unit.Module (
    tests
  ) where

import Test.Tasty.TestSet (group)

import Bead.Config (initTaskAssertions)
import Bead.Config.Parser (parseTests)
import Bead.Controller.Pages (pageDescTest)
import Bead.Daemon.Logout (logoutQueueTests)
import Bead.Domain.Entities (asgTests, entityTests, feedbackTests)
import Bead.Domain.Relationships (relationshipTests)
import Bead.Domain.RolePermission (permissionTest)
import Bead.Persistence.NoSQLDirFile (noSqlDirTests)
import Bead.Persistence.Persist (persistTests)
import Bead.Persistence.Relations (persistRelationsTests)
import Bead.View.Content.All (pageContentTest)
import Bead.View.Content.Home.Page (sumBinaryResultTests, sumPercentageResultTests)
import Bead.View.DataBridge (dataBridgeTests)
#ifdef EmailEnabled
import Bead.View.EmailTemplate (runEmailTemplateTests)
#endif
import Bead.View.Headers.AcceptLanguage (acceptLanguageTests)
import Bead.View.Pagelets (linkTextTest)
import Bead.View.RouteOf (routeOfTest)
import Bead.View.Routing (routingTest)
import Bead.View.Session (uniqueSessionKeysTest)
import Bead.View.TemplateAndComponentNames (fieldNameTest)
#ifdef EmailEnabled
import Bead.View.Validators (emailAddressTests)
#endif

tests = do
  group "Page description" pageDescTest
  group "Route of" routeOfTest
  group "Routing" routingTest
  group "Page content handler " pageContentTest
  group "Link text" linkTextTest
  group "Logout daemon" logoutQueueTests
  group "Permissions" permissionTest
  group "NoSQLDir" noSqlDirTests
  group "Field name" fieldNameTest
  group "Entity" entityTests
  group "Relationships" relationshipTests
  group "Unique session keys" uniqueSessionKeysTest
  group "Assignment" asgTests
  group "Homepage binary results" sumBinaryResultTests
  group "Homepage percentage results" sumPercentageResultTests
  group "Command line and configuration" initTaskAssertions
  group "Persist" persistTests
#ifdef EmailEnabled
  group "Email address" emailAddressTests
  group "Run email template" runEmailTemplateTests
#endif
  group "Persist relations" persistRelationsTests
  group "Data bridge" dataBridgeTests
  group "Feedback" feedbackTests
  group "Parse" parseTests
  group "Accept language" acceptLanguageTests
