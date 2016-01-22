{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.ScoreInfo (scoreInfoToIcon,scoreInfoToIconLink) where

import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Data.String (fromString)

import           Bead.View.Content hiding (notFound)
import           Bead.View.Content.VisualConstants
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.Domain.Shared.Evaluation

evResultToIcon :: I18N -> EvResult -> Html
evResultToIcon msg = evResultCata (binaryCata (resultCata passed' failed')) percentage free'
  where 
    passed' = passed msg
    failed' = failed msg
    free' = free msg 

scoreInfoToIcon :: I18N -> ScoreInfo -> Html
scoreInfoToIcon msg = scoreInfoAlgebra notFound' $ \_ek -> evResultToIcon msg
  where
    notFound' = notFound msg


scoreInfoToIconLink :: I18N -> String -> String -> ScoreInfo -> Html
scoreInfoToIconLink msg notFoundLink foundLink =
  scoreInfoAlgebra (linkWithHtml notFoundLink notFound') $ \_ek -> (linkWithHtml foundLink . evResultToIcon msg)
  where 
    notFound' = notFound msg

tooltip :: I18N -> Translation String -> H.Attribute
tooltip msg = A.title . fromString . msg

notFound :: I18N -> Html
notFound msg = H.i ! A.class_ "glyphicon glyphicon-stop" ! A.style "color:#AAAAAA; font-size: xx-large"
                   ! tooltip msg (msg_Home_SubmissionCell_NonEvaluated "Non evaluated") $ mempty

passed :: I18N -> Html
passed msg = H.i ! A.class_ "glyphicon glyphicon-thumbs-up" ! A.style "color:#00FF00; font-size: xx-large"
                 ! tooltip msg (msg_Home_SubmissionCell_Accepted "Accepted") $ mempty -- accepted

failed :: I18N -> Html
failed msg = H.i ! A.class_ "glyphicon glyphicon-thumbs-down" ! A.style "color:#FF0000; font-size: xx-large"
                 ! tooltip msg (msg_Home_SubmissionCell_Rejected "Rejected") $ mempty -- rejected

percentage :: Percentage -> Html
percentage (Percentage (Scores [p])) = H.span ! A.class_ "label label-primary" $ fromString $ percent p
percentage _ = error "SubmissionTable.coloredSubmissionCell percentage is not defined"

free :: I18N -> FreeForm -> Html
free msg = freeForm $ \msg ->
  let cell = if length msg < displayableFreeFormResultLength then msg else "..." in
  H.span ! A.class_ "label label-primary"
         ! A.title (fromString msg) $ (fromString cell)


percent :: RealFrac a => a -> String
percent x = concat [show . round $ (100 * x), "%"]
