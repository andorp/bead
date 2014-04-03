{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.SeeMore where

import           Data.String

import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Bead.View.Snap.Content

seeMorePre :: I18N -> Int -> Int -> String -> Html
seeMorePre i18n maxLength maxLines content = do
  previewWithMoreButton
  contentPreTag $ fromString content
  where
    hookClass' = fromString . hookClass
    isLargeContent = length content > maxLength

    previewWithMoreButton =
      if isLargeContent
        then do
          let cmt = take maxLength $ content
              ml  = unlines $ take maxLines $ lines cmt
              preview = concat $ [take (length ml - 1) ml, " ..."]
          H.span ! A.class_ (hookClass' seeMoreClass) # display False $ fromString . i18n $ Msg_SeeMore_SeeMore "See More"
          H.span ! A.class_ (hookClass' seeLessClass) # display False $ fromString . i18n $ Msg_SeeMore_SeeLess "See Less"
          H.pre ! A.class_ (hookClass' moreClass) # commentTextPre $ fromString preview
          H.a ! A.class_ (hookClass' moreButtonClass) # color "blue" $ fromString . i18n $ Msg_SeeMore_SeeMore "See More"
        else return ()

    contentPreTag =
      if isLargeContent
        then H.pre ! A.class_ (hookClass' moreClass) # (commentTextPre <> display False)
        else H.pre # commentTextPre
