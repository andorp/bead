{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.Config.Parser where

import Control.Applicative

import Data.ByteString.Char8 (ByteString, pack)
import Data.String
import Data.Yaml

import Bead.Config.Configuration

#ifdef TEST
import Test.Themis.Test
import Test.Themis.Test.Asserts
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
  parseJSON _ = error "Config is not parsed"

#ifdef LDAPEnabled
instance FromJSON LDAPLoginConfig where
  parseJSON (Object v) = LDAPLoginConfig
    <$> v .:? "non-ldap-user-file"
    <*> v .:  "ticket-temporary-dir"
    <*> v .:  "login-time-out-in-sec"
    <*> v .:  "no-of-login-threads"
    <*> v .:  "uid-key"
    <*> v .:  "name-key"
    <*> v .:  "email-key"
  parseJSON _ = error "LDAP login config is not parsed"
#else
instance FromJSON StandaloneLoginConfig where
  parseJSON (Object v) = StandaloneLoginConfig
    <$> v .: "username-regexp"
    <*> v .: "username-regexp-example"
  parseJSON _ = error "Standalone login config is not parsed"
#endif

parseYamlConfig :: String -> Either String Config
parseYamlConfig = decodeEither . pack

#ifdef TEST
parseTests = group "parserTests" $ do

#ifdef LDAPEnabled
  let ldapConfig1 = LDAPLoginConfig Nothing "/tmp/" 5 4 "uid" "name" "email"
  assertEquals "LDAP login config #1" (Right ldapConfig1)
    (decodeEither $ fromString $ unlines [
        "ticket-temporary-dir: '/tmp/'",
        "login-time-out-in-sec: 5",
        "no-of-login-threads: 4",
        "uid-key: 'uid'",
        "name-key: 'name'",
        "email-key: 'email'"
      ])
    "LDAP config is not parsed correctly"

  let ldapConfig2 = LDAPLoginConfig (Just "users.file") "/tmp/" 5 4 "uid" "name" "email"
  assertEquals "LDAP login config #2" (Right ldapConfig2)
    (decodeEither $ fromString $ unlines [
        "non-ldap-user-file: 'users.file'",
        "ticket-temporary-dir: '/tmp/'",
        "login-time-out-in-sec: 5",
        "no-of-login-threads: 4",
        "uid-key: 'uid'",
        "name-key: 'name'",
        "email-key: 'email'"
      ])
    "LDAP config is not parsed correctly"
#else
  let standaloneConfig1 = StandaloneLoginConfig "REGEXP" "REGEXP-EXAMPLE"
  assertEquals "Standalone login config" (Right standaloneConfig1)
    (decodeEither $ fromString $ unlines [
        "username-regexp: 'REGEXP'",
        "username-regexp-example: 'REGEXP-EXAMPLE'"
      ])
    "Standalone config is not parsed correctly"
#endif

  let configStr cfg = unlines [
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
          cfg
        ]

  let config lcf = Config {
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
       }

#ifdef LDAPEnabled
  assertEquals "Config with LDAP #1"
    (Right . config $ ldapConfig1)
    (parseYamlConfig . fromString . configStr $ unlines [
        "  ticket-temporary-dir: '/tmp/'",
        "  login-time-out-in-sec: 5",
        "  no-of-login-threads: 4",
        "  uid-key: 'uid'",
        "  name-key: 'name'",
        "  email-key: 'email'"
      ])
    "Config with LDAP is not parsed correctly"

  assertEquals "Config with LDAP #2"
    (Right . config $ ldapConfig2)
    (parseYamlConfig . fromString . configStr $ unlines [
        "  non-ldap-user-file: 'users.file'",
        "  ticket-temporary-dir: '/tmp/'",
        "  login-time-out-in-sec: 5",
        "  no-of-login-threads: 4",
        "  uid-key: 'uid'",
        "  name-key: 'name'",
        "  email-key: 'email'"
      ])
    "Config with LDAP is not parsed correctly"
#else
  assertEquals "Config with standalone"
    (Right . config $ standaloneConfig1)
    (parseYamlConfig . fromString . configStr $ unlines [
        "  username-regexp: 'REGEXP'",
        "  username-regexp-example: 'REGEXP-EXAMPLE'"
      ])
    "Config with standalone is not parsed correctly"
#endif
#endif
