module Bead.View.Content.Assessment.Page (
    newGroupAssessment
  , newCourseAssessment
  , fillGroupAssessmentPreview
  , fillCourseAssessmentPreview
  , viewAssessment
  ) where

import           Bead.View.Content

-- * Content Handlers

newGroupAssessment = ViewModifyHandler newGroupAssessmentPage postNewGroupAssessment
newCourseAssessment = ViewModifyHandler newCourseAssessmentPage postNewCourseAssessment
fillGroupAssessmentPreview = UserViewHandler fillGroupAssessmentPreviewPage
fillCourseAssessmentPreview = UserViewHandler fillGroupAssessmentPreviewPage
viewAssessment = ViewHandler viewAssessmentPage

newGroupAssessmentPage :: GETContentHandler
newGroupAssessmentPage = error "newGroupAssessmentPage is undefined"

postNewGroupAssessment :: POSTContentHandler
postNewGroupAssessment = error "postNewGroupAssessment is undefined"

newCourseAssessmentPage :: GETContentHandler
newCourseAssessmentPage = error "newCourseAssessmentPage is undefined"

postNewCourseAssessment :: POSTContentHandler
postNewCourseAssessment = error "postNewCourseAssessment is undefined"

fillGroupAssessmentPreviewPage :: ViewPOSTContentHandler
fillGroupAssessmentPreviewPage = error "fillGroupAssessmentPreviewPage is undefined"

fillCourseAssessmentPreviewPage :: ViewPOSTContentHandler
fillCourseAssessmentPreviewPage = error "fillCourseAssessmentPreviewPage is undefined"

viewAssessmentPage :: GETContentHandler
viewAssessmentPage = error "viewAssessmentPage is undefined"
