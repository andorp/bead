{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.SeeMore where

import           Data.String

import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap

seeMoreSubmission :: String -> I18N -> Int -> Int -> String -> Html
seeMoreSubmission id_ i18n maxLength maxLines content = do
  Bootstrap.panelGroup ! Bootstrap.role "tablist" ! Bootstrap.areaMultiselectable "true" $ do
    let headingId = "heading" ++ id_
    let collapseClass = if collapsed then "panel-collapse collapse"
                                     else "panel-collapse collapse in"
    let headerText = if collapsed then msg_Submission_Large_Submission "The submission is too long, click here to show"
                                  else msg_Submission_Collapse_Submission "Collapse submission text"
    H.div ! A.class_ "panel panel-default" $ do
      H.div ! A.class_ "panel-heading" ! Bootstrap.role "tab" ! A.id (fromString headingId) $ do
        H.h4 ! A.class_ "panel-title" $
          H.a ! Bootstrap.dataToggle "collapse" ! A.href (fromString $ '#':id_)
              ! Bootstrap.ariaExpanded "true" ! Bootstrap.ariaControls (fromString id_)
              $ fromString . i18n $ headerText
      H.div ! A.id (fromString id_) ! A.class_ (fromString collapseClass)
            ! Bootstrap.role "tabpanel" ! Bootstrap.ariaLabelledBy (fromString headingId) $
        H.div ! A.class_ "panel-body" $
          H.div # assignmentTextDiv $
            H.pre # assignmentTextPre $ fromString content
  where
    cmt = take maxLength $ content
    ml  = unlines $ take maxLines $ lines cmt
    isLargeContent = length content > maxLength
    collapsed = isLargeContent


seeMoreComment :: String -> I18N -> Int -> Int -> (String, Maybe Bootstrap.Alert) -> String -> Html
seeMoreComment id_ i18n maxLength maxLines (badgeText, alert) content =
  let headingId = "heading" ++ id_
      collapseClass = if collapsed then "panel-collapse collapse"
                                   else "panel-collapse collapse in"
  in
  H.div ! A.class_ "panel panel-default" $ do
    H.div ! A.class_ "panel-heading" ! Bootstrap.role "tab" ! A.id (fromString headingId) $ do
      H.p $ badge badgeText
      H.pre # commentTextPre $ fromString preview
      when isLargeContent $ do
        H.a ! Bootstrap.dataToggle "collapse" ! A.href (fromString $ '#':id_)
            ! Bootstrap.ariaExpanded "true" ! Bootstrap.ariaControls (fromString id_)
            $ fromString . i18n $ msg_SeeMore_SeeMore "See More"
    when isLargeContent $
      H.div ! A.id (fromString id_) ! A.class_ (fromString collapseClass)
            ! Bootstrap.role "tabpanel" ! Bootstrap.ariaLabelledBy (fromString headingId) $
        H.div ! A.class_ "panel-body" $
          H.pre # commentTextPre $ fromString content
  where
    badge = maybe Bootstrap.badge Bootstrap.badgeAlert alert
    cmt = take maxLength $ content
    ml  = unlines $ take maxLines $ lines cmt
    preview = if isLargeContent then concat $ [take (length ml - 1) ml, " ..."]
                                else content
    isLargeContent = length content > maxLength
    collapsed = isLargeContent
