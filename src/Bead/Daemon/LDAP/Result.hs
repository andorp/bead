module Bead.Daemon.LDAP.Result where

import Bead.Domain.Entities

data LDAPResult
  = LDAPError String
  | LDAPInvalidUser
  | LDAPAttrMapError
  | LDAPUser (Uid, Email, String)
  deriving (Eq, Show)

ldapResult
  ldapError
  ldapInvalidUser
  ldapAttrMapError
  ldapUser
  l = case l of
    LDAPError msg -> ldapError msg
    LDAPInvalidUser -> ldapInvalidUser
    LDAPAttrMapError -> ldapAttrMapError
    LDAPUser user -> ldapUser user
