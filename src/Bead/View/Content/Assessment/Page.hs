{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Assessment.Page (
    newGroupAssessment
  , newCourseAssessment
  , fillNewGroupAssessment
  , fillNewGroupAssessmentPreview
  , fillNewCourseAssessment
  , fillNewCourseAssessmentPreview
  , viewAssessment
  ) where

import           Bead.View.Content
import           Bead.View.RequestParams
import qualified Bead.Controller.Pages as Pages
import           Bead.Domain.Shared.Evaluation (binaryConfig)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Data.String (fromString)
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A


-- * Content Handlers

newGroupAssessment = ViewModifyHandler newGroupAssessmentPage postNewGroupAssessment
newCourseAssessment = ViewModifyHandler newCourseAssessmentPage postNewCourseAssessment
fillNewGroupAssessment = UserViewHandler fillNewGroupAssessmentPage
fillNewGroupAssessmentPreview = UserViewHandler fillNewGroupAssessmentPreviewPage
fillNewCourseAssessment = UserViewHandler fillNewCourseAssessmentPage
fillNewCourseAssessmentPreview = UserViewHandler fillNewCourseAssessmentPreviewPage
viewAssessment = ViewHandler viewAssessmentPage

data PageDataNew  = PD_NewCourseAssessment CourseKey
                  | PD_NewGroupAssessment  GroupKey
data PageDataFill = PD_FillCourseAssessment CourseKey String String
                  | PD_FillGroupAssessment  GroupKey  String String 
                  | PD_PreviewCourseAssessment CourseKey String String
                  | PD_PreviewGroupAssessment GroupKey String String

newGroupAssessmentPage :: GETContentHandler
newGroupAssessmentPage = do
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  return $ newAssessmentTemplate $ PD_NewGroupAssessment gk

postNewGroupAssessment :: POSTContentHandler
postNewGroupAssessment = do 
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  let a = Assessment title description binaryConfig
  return $ CreateGroupAssessment gk a

newCourseAssessmentPage :: GETContentHandler
newCourseAssessmentPage = do
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  return $ newAssessmentTemplate $ PD_NewCourseAssessment ck

postNewCourseAssessment :: POSTContentHandler
postNewCourseAssessment = do 
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  let a = Assessment title description binaryConfig
  return $ CreateCourseAssessment ck a

fillNewGroupAssessmentPage :: ViewPOSTContentHandler
fillNewGroupAssessmentPage = do
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  gk <- getParameter $ customGroupKeyPrm groupKeyParamName
  return $ fillAssessmentTemplate $ PD_FillGroupAssessment gk title description

titleParam = stringParameter "n1" "Title"
descriptionParam = stringParameter "n2" "Description"

fillNewCourseAssessmentPage :: ViewPOSTContentHandler
fillNewCourseAssessmentPage = do
  title <- getParameter titleParam
  description <- getParameter descriptionParam
  ck <- getParameter $ customCourseKeyPrm courseKeyParamName
  return $ fillAssessmentTemplate $ PD_FillCourseAssessment ck title description

fillNewGroupAssessmentPreviewPage :: ViewPOSTContentHandler
fillNewGroupAssessmentPreviewPage = error "fillNewGroupAssessmentPreviewPage is undefined"

fillNewCourseAssessmentPreviewPage :: ViewPOSTContentHandler
fillNewCourseAssessmentPreviewPage = error "fillNewCourseAssessmentPreviewPage is undefined"

fillAssessmentTemplate :: PageDataFill -> IHtml
fillAssessmentTemplate pdata = do
  _msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ do      
      postForm (routeOf assessment) $ do
        Bootstrap.textInputWithDefault "n1" "Title" title
        Bootstrap.textInputWithDefault "n2" "Description" description
        fileInput ("as")
        Bootstrap.row $ do
             let formAction page = onclick (fromString $ concat ["javascript: form.action='", routeOf page, "';"])
                 downloadCsvButton = Bootstrap.blockButtonLink
                   (routeOf getCsv)
                   "Get CSV"
             Bootstrap.colMd6 $ downloadCsvButton
             Bootstrap.colMd6 $ Bootstrap.submitButtonWithAttr (formAction $ assessment) "Preview"

  where
    (title,description) = case pdata of
                            PD_FillCourseAssessment _ck title description -> (title,description)
                            PD_FillGroupAssessment _gk title description -> (title,description)
    assessment = case pdata of
                   PD_FillCourseAssessment ck _title _description -> Pages.fillNewCourseAssessmentPreview ck ()
                   PD_FillGroupAssessment gk _title _description -> Pages.fillNewGroupAssessmentPreview gk ()
    getCsv = case pdata of
             PD_FillCourseAssessment ck _title _description -> Pages.getCourseCsv ck ()
             PD_FillGroupAssessment gk _title _description -> Pages.getGroupCsv gk ()

viewAssessmentPage :: GETContentHandler
viewAssessmentPage = error "viewAssessmentPage is undefined"
                     
newAssessmentTemplate :: PageDataNew -> IHtml
newAssessmentTemplate pdata = do
  _msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ do      
      postForm (routeOf assessment) $ do
        Bootstrap.textInput "n1" "Title" ""
        Bootstrap.textInput "n2" "Description" ""
        Bootstrap.row $ do
             let formAction page = onclick (fromString $ concat ["javascript: form.action='", routeOf page, "';"])
             Bootstrap.colMd6 $ Bootstrap.submitButtonWithAttr (formAction $ fill) "Fill"
             Bootstrap.colMd6 $ Bootstrap.submitButtonWithAttr (formAction $ assessment) "Commit"

  where
    assessment = case pdata of
                   PD_NewCourseAssessment ck -> Pages.newCourseAssessment ck ()
                   PD_NewGroupAssessment gk  -> Pages.newGroupAssessment gk ()
    fill = case pdata of
             PD_NewCourseAssessment ck -> Pages.fillNewCourseAssessment ck ()
             PD_NewGroupAssessment gk  -> Pages.fillNewGroupAssessment gk ()

