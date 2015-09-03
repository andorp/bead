{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.Config.Parser where

import Control.Applicative

import Data.ByteString.Char8 (pack)
import Data.Maybe
import Data.String
import Data.Yaml

import Bead.Config.Configuration

#ifdef TEST
import Test.Tasty.TestSet
#endif

-- * JSON instances

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v.: "user-actions-log"
    <*> v.: "session-timeout"
#ifdef EmailEnabled
    <*> v.: "hostname-for-emails"
    <*> v.: "from-email-address"
#endif
    <*> v.: "default-login-language"
    <*> v.: "default-timezone"
    <*> v.: "timezone-info-directory"
    <*> v.: "max-upload-size"
    <*> v.: "login-config"
#ifdef MYSQL
    <*> v.: "mysql-config"
#else
    <*> pure FilePersistConfig
#endif
  parseJSON _ = error "Config is not parsed"

#ifdef SSO
instance FromJSON SSOLoginConfig where
  parseJSON (Object v) = SSOLoginConfig
    <$> (withDefault 5  <$> v .:? "timeout")
    <*> (withDefault 4  <$> v .:? "threads")
    <*> (withDefault "ldapsearch -Q -LLL" <$> v .:? "query-command")
    <*> v .: "uid-key"
    <*> v .: "name-key"
    <*> v .: "email-key"
    <*> (withDefault False <$> v .:? "developer")
    where
      withDefault = flip maybe id

  parseJSON _ = error "SSO login config is not parsed"
#else
instance FromJSON StandaloneLoginConfig where
  parseJSON (Object v) = StandaloneLoginConfig
    <$> v .: "username-regexp"
    <*> v .: "username-regexp-example"
  parseJSON _ = error "Standalone login config is not parsed"
#endif

#ifdef MYSQL
instance FromJSON MySQLConfig where
  parseJSON (Object v) = MySQLConfig
    <$> v.: "database"
    <*> v.: "hostname"
    <*> v.: "port"
    <*> v.: "username"
    <*> v.: "password"
    <*> v.: "pool-size"
  parseJSON _ = error "MySQL config is not parsed"
#endif

parseYamlConfig :: String -> Either String Config
parseYamlConfig = decodeEither . pack

#ifdef TEST
parseTests = group "parserTests" $ do

#ifdef SSO
  let sSOConfig1 = SSOLoginConfig 5 4 "ldapsearch -Q -LLL" "uid" "name" "email" False
  assertEquals "SSO login config #1" (Right sSOConfig1)
    (decodeEither $ fromString $ unlines [
        "uid-key: 'uid'",
        "name-key: 'name'",
        "email-key: 'email'"
      ])
    "SSO config is not parsed correctly"

  let sSOConfig2 = SSOLoginConfig 5 4 "ldapsearch -Q" "uid" "name" "email" True
  assertEquals "SSO login config #2" (Right sSOConfig2)
    (decodeEither $ fromString $ unlines [
        "timeout: 5",
        "threads: 4",
        "query-command: 'ldapsearch -Q'",
        "uid-key: 'uid'",
        "name-key: 'name'",
        "email-key: 'email'",
        "developer: yes"
      ])
    "SSO config is not parsed correctly"
#else
  let standaloneConfig1 = StandaloneLoginConfig "REGEXP" "REGEXP-EXAMPLE"
  assertEquals "Standalone login config" (Right standaloneConfig1)
    (decodeEither $ fromString $ unlines [
        "username-regexp: 'REGEXP'",
        "username-regexp-example: 'REGEXP-EXAMPLE'"
      ])
    "Standalone config is not parsed correctly"
#endif

  let configStr loginCfg persistCfg = unlines [
          "user-actions-log: 'actions'",
          "session-timeout: 10",
#ifdef EmailEnabled
          "hostname-for-emails: 'www.google.com'",
          "from-email-address: 'mail@google.com'",
#endif
          "default-login-language: 'en'",
          "default-timezone: 'Europe/Budapest'",
          "timezone-info-directory: '/opt/some'",
          "max-upload-size: 150",
          "login-config:",
          loginCfg,
#ifdef MYSQL
          "mysql-config:",
          persistCfg,
#endif
          ""
        ]

  let config lcf pcfg = Config {
         userActionLogFile = "actions"
       , sessionTimeout = 10
#ifdef EmailEnabled
       , emailHostname = "www.google.com"
       , emailFromAddress = "mail@google.com"
#endif
       , defaultLoginLanguage = "en"
       , defaultRegistrationTimezone = "Europe/Budapest"
       , timeZoneInfoDirectory = "/opt/some"
       , maxUploadSizeInKb = 150
       , loginConfig = lcf
       , persistConfig = pcfg
       }

  let persistConfig =
#ifdef MYSQL
       MySQLConfig {
           mySQLDbName   = "bead-test-db"
         , mySQLHost     = "mysql.server.com"
         , mySQLPort     = 3308
         , mySQLUser     = "bead"
         , mySQLPass     = "secret"
         , mySQLPoolSize = 30
         }
#else
       FilePersistConfig
#endif

  let persistConfigStr =
#ifdef MYSQL
        unlines [
          "    database: bead-test-db",
          "    hostname: mysql.server.com",
          "    port: 3308",
          "    username: bead",
          "    password: secret",
          "    pool-size: 30"
        ]
#else
        ""
#endif

#ifdef SSO
  assertEquals "Config with SSO #1"
    (Right $ config sSOConfig1 persistConfig)
    (parseYamlConfig . fromString $ configStr (unlines [
        "  uid-key: 'uid'",
        "  name-key: 'name'",
        "  email-key: 'email'"
      ]) persistConfigStr)
    "Config with SSO is not parsed correctly"

  assertEquals "Config with SSO #2"
    (Right $ config sSOConfig2 persistConfig)
    (parseYamlConfig . fromString $ configStr (unlines [
        "  timeout: 5",
        "  threads: 4",
        "  query-command: 'ldapsearch -Q'",
        "  uid-key: 'uid'",
        "  name-key: 'name'",
        "  email-key: 'email'",
        "  developer: yes"
      ]) persistConfigStr)
    "Config with SSO is not parsed correctly"
#else
  assertEquals "Config with standalone"
    (Right $ config standaloneConfig1 persistConfig)
    (parseYamlConfig . fromString $ configStr
      (unlines [
        "  username-regexp: 'REGEXP'",
        "  username-regexp-example: 'REGEXP-EXAMPLE'"])
      persistConfigStr)
    "Config with standalone is not parsed correctly"
#endif

#endif
