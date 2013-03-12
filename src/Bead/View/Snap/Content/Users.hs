module Bead.View.Snap.Content.Users (
    users
  ) where

import Bead.View.Snap.Content
import Bead.Domain.Entities (User(..))
import Bead.Controller.UserStories (selectUsers)
import Bead.Controller.Pages as P (Page(UserDetails))

users :: Content
users = getContentHandler listUsers

listUsers :: GETContentHandler
listUsers = withUserStateE $ \s -> do
  users <- runStoryE . selectUsers $ (const True)
  lift $ blaze $ withUserFrame s (userKeys (routeOf P.UserDetails) (map u_username users)) Nothing


