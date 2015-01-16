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
    <*> v.: "hostname-for-emails"
    <*> v.: "from-email-address"
    <*> v.: "default-login-language"
    <*> v.: "timezone-info-directory"
    <*> v.: "max-upload-size"
    <*> v.: "login-config"
  parseJSON _ = error "Config is not parsed"

instance FromJSON LoginCfg where
  parseJSON (Object v) =
    (LDAPLC <$> v .: "ldap") <|>
    (STDLC  <$> v .: "standalone")
  parseJSON _ = error "Login config is not parsed"

instance FromJSON LDAPLoginConfig where
  parseJSON (Object v) = LDAPLoginConfig
    <$> v .:? "non-ldap-user-file"
    <*> v .:  "default-timezone"
  parseJSON _ = error "LDAP login config is not parsed"

instance FromJSON StandaloneLoginConfig where
  parseJSON (Object v) = StandaloneLoginConfig
    <$> v .: "username-regexp"
    <*> v .: "username-regexp-example"
  parseJSON _ = error "Standalone login config is not parsed"

parseYamlConfig :: String -> Either String Config
parseYamlConfig = decodeEither . pack

#ifdef TEST
parseTests = group "parserTests" $ do

  let standaloneConfig1 = StandaloneLoginConfig "REGEXP" "REGEXP-EXAMPLE"
  assertEquals "Standalone login config" (Right standaloneConfig1)
    (decodeEither $ fromString $ unlines [
        "username-regexp: 'REGEXP'",
        "username-regexp-example: 'REGEXP-EXAMPLE'"
      ])
    "Standalone config is not parsed correctly"

  let ldapConfig1 = LDAPLoginConfig Nothing "Europe/Budapest"
  assertEquals "LDAP login config #1" (Right ldapConfig1)
    (decodeEither $ fromString $ unlines [
        "default-timezone: 'Europe/Budapest'"
      ])
    "LDAP config is not parsed correctly"

  let ldapConfig2 = LDAPLoginConfig (Just "users.file") "Europe/Budapest"
  assertEquals "LDAP login config #2" (Right ldapConfig2)
    (decodeEither $ fromString $ unlines [
        "non-ldap-user-file: 'users.file'",
        "default-timezone: 'Europe/Budapest'"
      ])
    "LDAP config is not parsed correctly"

  let configStr cfg = unlines [
          "user-actions-log: 'actions'",
          "session-timeout: 10",
          "hostname-for-emails: 'www.google.com'",
          "from-email-address: 'mail@google.com'",
          "default-login-language: 'en'",
          "timezone-info-directory: '/opt/some'",
          "max-upload-size: 150",
          "login-config:",
          cfg
        ]

  let config lcf = Config {
         userActionLogFile = "actions"
       , sessionTimeout = 10
       , emailHostname = "www.google.com"
       , emailFromAddress = "mail@google.com"
       , defaultLoginLanguage = "en"
       , timeZoneInfoDirectory = "/opt/some"
       , maxUploadSizeInKb = 150
       , loginConfig = lcf
       }

  assertEquals "Config with LDAP #1"
    (Right . config $ LDAPLC ldapConfig1)
    (parseYamlConfig . fromString . configStr $ unlines [
        "  ldap:",
        "    default-timezone: 'Europe/Budapest'"
      ])
    "Config with LDAP is not parsed correctly"

  assertEquals "Config with LDAP #2"
    (Right . config $ LDAPLC ldapConfig2)
    (parseYamlConfig . fromString . configStr $ unlines [
        "  ldap:",
        "    non-ldap-user-file: 'users.file'",
        "    default-timezone: 'Europe/Budapest'"
      ])
    "Config with LDAP is not parsed correctly"

  assertEquals "Config with standalone"
    (Right . config $ STDLC standaloneConfig1)
    (parseYamlConfig . fromString . configStr $ unlines [
        "  standalone:",
        "    username-regexp: 'REGEXP'",
        "    username-regexp-example: 'REGEXP-EXAMPLE'"
      ])
    "Config with standalone is not parsed correctly"

#endif
