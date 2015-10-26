{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Comments (
    CommentOrFeedback
  , submissionDescToCFs
  , submissionDetailsDescToCFs
  , commentsToCFs
  , commentsDiv
  , commentPostForm
  , feedbacksToCFs
  , forStudentCFs
  ) where

import           Data.List (sortBy)
import           Data.String
import           Control.Monad

import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Printf

import           Bead.Controller.Pages as Pages
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.SeeMore
import           Bead.Domain.Shared.Evaluation

type CommentOrFeedback = Either Comment Feedback

commentOrFeedback
  comment
  feedback
  cf = case cf of
    Left  c -> comment  c
    Right f -> feedback f

commentOrFeedbackTime = commentOrFeedback commentDate postDate

commentsToCFs :: [Comment] -> [CommentOrFeedback]
commentsToCFs = map Left

feedbacksToCFs :: [Feedback] -> [CommentOrFeedback]
feedbacksToCFs = map Right

-- Converts a given submission description into a list of comments and feedbacks
submissionDescToCFs :: SubmissionDesc -> [CommentOrFeedback]
submissionDescToCFs s = (commentsToCFs $ eComments s) ++ (feedbacksToCFs $ eFeedbacks s)

-- Converts a given submission detailed description into a list of comments and feedbacks
submissionDetailsDescToCFs :: SubmissionDetailsDesc -> [CommentOrFeedback]
submissionDetailsDescToCFs s = (commentsToCFs $ sdComments s) ++ (feedbacksToCFs $ sdFeedbacks s)

-- Sort the items by increasing by the creation time
sortIncreasingTime :: [CommentOrFeedback] -> [CommentOrFeedback]
sortIncreasingTime = sortBy compareTimes where
  compareTimes a b = compare (time a) (time b)
  time = commentOrFeedbackTime

-- Sort the items by descreasing by the creation time
sortDecreasingTime = reverse . sortIncreasingTime

-- Filters out the comments and feedback not visible for the students
forStudentCFs :: [CommentOrFeedback] -> [CommentOrFeedback]
forStudentCFs = filter forStudent where
  forStudent =
    commentOrFeedback
      isStudentComment
      (feedback
         (feedbackInfo
           (const True) -- result
           (const True) -- student
           (const False) -- admin
           (const3 True)) -- evaluated
         p_1_2)

commentsDiv :: String -> UserTimeConverter -> [CommentOrFeedback] -> IHtml
commentsDiv id_ t cs = do
  msg <- getI18N
  return $ Bootstrap.panelGroup ! Bootstrap.role "tablist" ! Bootstrap.areaMultiselectable "true" $ do
    mapM_ (commentPar msg id_ t) $ zip [1..] (sortDecreasingTime cs)

commentPar :: I18N -> String -> UserTimeConverter -> (Int, CommentOrFeedback) -> Html
commentPar i18n id_ t (n, c) = do
  let comment = commentText c
  let badge = concat [showDate . t $ commentOrFeedbackTime c, " ", commentAuthor c]
  let commentId = fromString $ id_ ++ show n
  seeMoreComment commentId i18n maxLength maxLines (badge, style) (commentText c)
  where
    commentText =
      commentOrFeedback
        (commentCata $ \comment _author _date _type -> comment)
        (feedback
           (feedbackInfo
             (bool testsPassed testsFailed) -- result
             id   -- comment
             id   -- comment
             evaluationText) -- evaluation
           p_1_2)
      where
        testsPassed = i18n $ msg_Comments_TestPassed "The submission has passed the tests."
        testsFailed = i18n $ msg_Comments_TestFailed "The submission has failed the tests."

        bool true false x = if x then true else false

        evaluationText result comment _author =
          withEvResult result
            (\b -> join [comment, "\n\n", translateMessage i18n (binaryResult b)])
            (const $ join [comment, "\n\n", translateMessage i18n (pctResult result)])
            (\(FreeForm msg) -> join [comment, "\n\n", msg])

        binaryResult (Binary b) =
          TransMsg $ resultCata (msg_Comments_BinaryResultPassed "The submission is accepted.")
                                (msg_Comments_BinaryResultFailed "The submission is rejected.")
                                b

        pctResult p = TransPrmMsg
          (msg_Comments_PercentageResult "The percentage of the evaluation: %s")
          (maybe "ERROR: Invalid percentage value! Please contact with the administrations"
                 doubleToPercentageStr $ percentValue p)
          where
            doubleToPercentageStr = printf "%.0f%%" . (100 *)

    commentAuthor =
      commentOrFeedback
        (commentCata $ \_comment author _date ->
           commentTypeCata
             author -- student
             author -- groupAdmin
             author -- courseAdmin
             author) -- admin
        (feedback
          (feedbackInfo
            (const result) -- result
            (const testScript) -- student
            (const adminTestScript) -- admin
            (\_result _comment author -> author)) -- evaluation
          p_1_2)
      where
        adminTestScript = i18n $ msg_Comments_AuthorTestScript_Private "Test Script (seen by only admins)"
        testScript = i18n $ msg_Comments_AuthorTestScript_Public "Test Script"
        result = testScript


    style =
      commentOrFeedback
        (commentCata (const4 Nothing))
        (feedback
          (feedbackInfo
            (const Nothing) -- result
            (const Nothing) -- student
            (const $ Just Bootstrap.Warning) -- admin
            (const3 $ Just Bootstrap.Warning)) -- evaluation
          p_1_2)
        c

    maxLength = 100
    maxLines = 5

-- Creates a post form for the given route assignment key and submission key, where
-- a comment can be placed and the result is submitted to the given page, which is
-- need to understand the given parameters
commentPostForm :: Page a b c d e -> AssignmentKey -> IHtml
commentPostForm p ak = do
  msg <- getI18N
  return $ postForm (routeOf p) $ do
    H.div ! formDiv $ do
      textAreaInput (fieldName commentValueField) Nothing ! fillDiv ! A.required ""
      hiddenInput (fieldName assignmentKeyField) (paramValue ak)
    H.br
    submitButton (fieldName commentBtn) (msg $ msg_Comments_SubmitButton "Submit")

-- * CSS section

fillDiv = A.style "width: 98%; height: 98%"
formDiv = A.style "width: 100%; height: 100px"
