module Bead.View.Snap.Content.UserDetails (
    userDetails
  ) where

import Bead.View.Snap.Content

userDetails :: Content
userDetails = getPostContentHandler userDetailPage userDataChange

userDetailPage :: GETContentHandler
userDetailPage = undefined

userDataChange :: POSTContentHandler
userDataChange = undefined

