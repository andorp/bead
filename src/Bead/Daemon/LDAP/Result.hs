module Bead.Daemon.LDAP.Result where

import Bead.Domain.Entities

data LDAPResult
  = LDAPError String
  | LDAPInvalidAuth
  | LDAPAttrMapError
  | LDAPUser (Uid, Email, String)
  deriving (Eq, Show)

ldapResult
  ldapError
  ldapInvalidAuth
  ldapAttrMapError
  ldapUser
  l = case l of
    LDAPError msg -> ldapError msg
    LDAPInvalidAuth -> ldapInvalidAuth
    LDAPAttrMapError -> ldapAttrMapError
    LDAPUser user -> ldapUser user
