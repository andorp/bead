{-# LANGUAGE FlexibleInstances #-}
module Bead.View.Snap.TranslationEnum (
    Enum(..)
  , Bounded(..)
  , Translation(..)
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Bead.View.Snap.Translation


translationList =
  [ Msg_Login_PageTitle ()
  , Msg_Login_Username ()
  , Msg_Login_Password ()
  , Msg_Login_Submit ()
  , Msg_Login_Title ()
  , Msg_Login_Registration ()
  , Msg_Login_Forgotten_Password ()
  , Msg_Login_InternalError ()
  , Msg_Login_SelectLanguage ()
  , Msg_Login_InvalidPasswordOrUser ()

  , Msg_Routing_InvalidRoute ()
  , Msg_Routing_SessionTimedOut ()

  , Msg_ErrorPage_Title ()
  , Msg_ErrorPage_GoBackToLogin ()
  , Msg_ErrorPage_Header ()

  , Msg_Input_Group_Name ()
  , Msg_Input_Group_Description ()
  , Msg_Input_Group_Evaluation ()
  , Msg_Input_Course_Name ()
  , Msg_Input_Course_Description ()
  , Msg_Input_Course_Evaluation ()
  , Msg_Input_Course_TestScript ()
  , Msg_Input_User_Role ()
  , Msg_Input_User_Email ()
  , Msg_Input_User_FullName ()
  , Msg_Input_User_TimeZone ()
  , Msg_Input_User_Language ()
  , Msg_Input_TestScriptSimple ()
  , Msg_Input_TestScriptZipped ()

  , Msg_CourseAdmin_CreateCourse ()
  , Msg_CourseAdmin_AssignAdmin ()
  , Msg_CourseAdmin_AssignAdmin_Button ()
  , Msg_CourseAdmin_CreateGroup ()
  , Msg_CourseAdmin_NoCourses ()
  , Msg_CourseAdmin_Course ()
  , Msg_CourseAdmin_PctHelpMessage ()
  , Msg_CourseAdmin_NoGroups ()
  , Msg_CourseAdmin_NoGroupAdmins ()
  , Msg_CourseAdmin_Group ()
  , Msg_CourseAdmin_Admin ()
  , Msg_CourseAdmin_GroupAdmins_Info ()
  , Msg_CourseAdmin_GroupAdmins_Group ()
  , Msg_CourseAdmin_GroupAdmins_Admins ()
  , Msg_Administration_NewCourse ()
  , Msg_Administration_PctHelpMessage ()
  , Msg_Administration_CreatedCourses ()
  , Msg_Administration_CreateCourse ()
  , Msg_Administration_AssignCourseAdminTitle ()
  , Msg_Administration_NoCourses ()
  , Msg_Administration_NoCourseAdmins ()
  , Msg_Administration_AssignCourseAdminButton ()
  , Msg_Administration_ChangeUserProfile ()
  , Msg_Administration_SelectUser ()
  , Msg_Administration_HowToAddMoreAdmins ()
  , Msg_Administration_CourseAdmins_Info ()
  , Msg_Administration_CourseAdmins_Course ()
  , Msg_Administration_CourseAdmins_Admins ()

  , Msg_NewAssignment_Title ()
  , Msg_NewAssignment_Title_Default ()
  , Msg_NewAssignment_SubmissionDeadline ()
  , Msg_NewAssignment_StartDate ()
  , Msg_NewAssignment_EndDate ()
  , Msg_NewAssignment_Description ()
  , Msg_NewAssignment_Description_Default ()
  , Msg_NewAssignment_Markdown ()
  , Msg_NewAssignment_CanBeUsed ()
  , Msg_NewAssignment_Properties ()
  , Msg_NewAssignment_Course ()
  , Msg_NewAssignment_Group ()
  , Msg_NewAssignment_SaveButton ()
  , Msg_NewAssignment_PreviewButton ()
  , Msg_NewAssignment_Title_Normal ()
  , Msg_NewAssignment_Title_BallotBox ()
  , Msg_NewAssignment_Title_Password ()
  , Msg_NewAssignment_Info_Normal ()
  , Msg_NewAssignment_Info_BallotBox ()
  , Msg_NewAssignment_Info_Password ()
  , Msg_NewAssignment_TestCase ()
  , Msg_NewAssignment_TestScripts ()
  , Msg_NewAssignment_DoNotOverwrite ()
  , Msg_NewAssignment_NoTesting ()
  , Msg_NewAssignment_TestFile ()
  , Msg_NewAssignment_TestFile_Info ()
  , Msg_NewAssignment_AssignmentPreview ()
  , Msg_NewAssignment_AssignmentPreview ()
  , Msg_NewAssignment_BallotBox ()
  , Msg_NewAssignment_PasswordProtected ()
  , Msg_NewAssignment_Password ()
  , Msg_NewAssignment_EvaluationType ()
  , Msg_NewAssignment_BinaryEvaluation ()
  , Msg_NewAssignment_PercentageEvaluation ()
  , Msg_NewAssignment_EvalTypeWarn ()
  , Msg_NewAssignment_Isolated ()
  , Msg_NewAssignment_Info_Isolated ()

  , Msg_GroupRegistration_RegisteredCourses ()
  , Msg_GroupRegistration_SelectGroup ()
  , Msg_GroupRegistration_NoRegisteredCourses ()
  , Msg_GroupRegistration_Courses ()
  , Msg_GroupRegistration_Admins ()
  , Msg_GroupRegistration_NoAvailableCourses ()
  , Msg_GroupRegistration_Register ()
  , Msg_GroupRegistration_Unsubscribe ()
  , Msg_GroupRegistration_NoUnsubscriptionAvailable ()
  , Msg_GroupRegistration_Warning ()

  , Msg_UserDetails_SaveButton ()
  , Msg_UserDetails_NonExistingUser ()

  , Msg_Submission_Course ()
  , Msg_Submission_Admin ()
  , Msg_Submission_Assignment ()
  , Msg_Submission_Deadline ()
  , Msg_Submission_Description ()
  , Msg_Submission_Solution ()
  , Msg_Submission_Submit ()
  , Msg_Submission_Invalid_Assignment ()
  , Msg_Submission_TimeLeft ()
  , Msg_Submission_Days ()
  , Msg_Submission_DeadlineReached ()
  , Msg_Submission_Info_Password ()
  , Msg_Submission_Info_File ()
  , Msg_Submission_File_NoFileReceived ()
  , Msg_Submission_File_PolicyFailure ()
  , Msg_Submission_File_InvalidFile ()
  , Msg_Submission_File_InternalError ()

  , Msg_Comments_Title ()
  , Msg_Comments_SubmitButton ()
  , Msg_Comments_AuthorTestScript_Private ()
  , Msg_Comments_AuthorTestScript_Public ()
  , Msg_Comments_TestPassed ()
  , Msg_Comments_TestFailed ()
  , Msg_Comments_BinaryResultPassed ()
  , Msg_Comments_BinaryResultFailed ()
  , Msg_Comments_PercentageResult ()

  , Msg_Evaluation_Title ()
  , Msg_Evaluation_Course ()
  , Msg_Evaluation_Group ()
  , Msg_Evaluation_Student ()
  , Msg_Evaluation_SaveButton ()
  , Msg_Evaluation_Submitted_Solution ()
  , Msg_Evaluation_Submitted_Solution_Text_Info ()
  , Msg_Evaluation_Submitted_Solution_Text_Link ()
  , Msg_Evaluation_Submitted_Solution_Zip_Info ()
  , Msg_Evaluation_Submitted_Solution_Zip_Link ()
  , Msg_Evaluation_Accepted ()
  , Msg_Evaluation_Rejected ()
  , Msg_Evaluation_New_Comment ()
  , Msg_Evaluation_Info ()
  , Msg_Evaluation_Username ()
  , Msg_Evaluation_SubmissionDate ()

  , Msg_SubmissionDetails_Course ()
  , Msg_SubmissionDetails_Admins ()
  , Msg_SubmissionDetails_Assignment ()
  , Msg_SubmissionDetails_Deadline ()
  , Msg_SubmissionDetails_Description ()
  , Msg_SubmissionDetails_Solution ()
  , Msg_SubmissionDetails_Solution_Text_Info ()
  , Msg_SubmissionDetails_Solution_Text_Link ()
  , Msg_SubmissionDetails_Solution_Zip_Info ()
  , Msg_SubmissionDetails_Solution_Zip_Link ()
  , Msg_SubmissionDetails_Evaluation ()
  , Msg_SubmissionDetails_NewComment ()
  , Msg_SubmissionDetails_SubmitComment ()
  , Msg_SubmissionDetails_InvalidSubmission ()

  , Msg_Registration_Title ()
  , Msg_Registration_Username ()
  , Msg_Registration_Email ()
  , Msg_Registration_FullName ()
  , Msg_Registration_SubmitButton ()
  , Msg_Registration_GoBackToLogin ()
  , Msg_Registration_InvalidUsername ()
  , Msg_Registration_HasNoUserAccess ()
  , Msg_Registration_UserAlreadyExists ()
  , Msg_Registration_RegistrationNotSaved ()
  , Msg_Registration_EmailSubject ()
  , Msg_Registration_EmailBody ()
  , Msg_Registration_RequestParameterIsMissing ()

  , Msg_RegistrationFinalize_NoRegistrationParametersAreFound ()
  , Msg_RegistrationFinalize_SomeError ()
  , Msg_RegistrationFinalize_InvalidToken ()
  , Msg_RegistrationFinalize_UserAlreadyExist ()
  , Msg_RegistrationFinalize_Password ()
  , Msg_RegistrationFinalize_PwdAgain ()
  , Msg_RegistrationFinalize_Timezone ()
  , Msg_RegistrationFinalize_SubmitButton ()
  , Msg_RegistrationFinalize_GoBackToLogin ()

  , Msg_RegistrationCreateStudent_NoParameters ()
  , Msg_RegistrationCreateStudent_InternalError ()
  , Msg_RegistrationCreateStudent_InvalidToken ()

  , Msg_RegistrationTokenSend_Title ()
  , Msg_RegistrationTokenSend_StoryFailed ()
  , Msg_RegistrationTokenSend_GoBackToLogin ()

  , Msg_EvaluationTable_EmptyUnevaluatedSolutions ()
  , Msg_EvaluationTable_Course ()
  , Msg_EvaluationTable_Group ()
  , Msg_EvaluationTable_Student ()
  , Msg_EvaluationTable_Assignment ()
  , Msg_EvaluationTable_Link ()
  , Msg_EvaluationTable_Solution ()
  , Msg_EvaluationTable_Info ()
  , Msg_EvaluationTable_CourseAssignment ()
  , Msg_EvaluationTable_GroupAssignment ()
  , Msg_EvaluationTable_MiscCourseAssignment ()
  , Msg_EvaluationTable_CourseAssignmentInfo ()
  , Msg_EvaluationTable_GroupAssignmentInfo ()
  , Msg_EvaluationTable_MiscCourseAssignmentInfo ()
  , Msg_EvaluationTable_Username ()
  , Msg_EvaluationTable_DateOfSubmission ()

  , Msg_UserSubmissions_NonAccessibleSubmissions ()
  , Msg_UserSubmissions_Course ()
  , Msg_UserSubmissions_Assignment ()
  , Msg_UserSubmissions_Student ()
  , Msg_UserSubmissions_SubmittedSolutions ()
  , Msg_UserSubmissions_SubmissionDate ()
  , Msg_UserSubmissions_Evaluation ()

  , Msg_UserSubmissions_Accepted ()
  , Msg_UserSubmissions_Rejected ()
  , Msg_UserSubmissions_NotFound ()
  , Msg_UserSubmissions_NonEvaluated ()
  , Msg_UserSubmissions_Tests_Passed ()
  , Msg_UserSubmissions_NonEvaluated ()
  , Msg_UserSubmissions_Tests_Failed ()

  , Msg_SubmissionList_CourseOrGroup ()
  , Msg_SubmissionList_Admin ()
  , Msg_SubmissionList_Assignment ()
  , Msg_SubmissionList_Deadline ()
  , Msg_SubmissionList_Description ()
  , Msg_SubmissionList_SubmittedSolutions ()

  , Msg_SubmissionList_NoSubmittedSolutions ()
  , Msg_SubmissionList_NonAssociatedAssignment ()
  , Msg_SubmissionList_NonReachableAssignment ()
  , Msg_SubmissionList_Info ()
  , Msg_SubmissionList_NotFound ()
  , Msg_SubmissionList_TestsPassed ()
  , Msg_SubmissionList_TestsFailed ()
  , Msg_SubmissionList_Passed ()
  , Msg_SubmissionList_Failed ()

  , Msg_ResetPassword_UserDoesNotExist ()
  , Msg_ResetPassword_PasswordIsSet ()
  , Msg_ResetPassword_GoBackToLogin ()
  , Msg_ResetPassword_Username ()
  , Msg_ResetPassword_Email ()
  , Msg_ResetPassword_NewPwdButton ()
  , Msg_ResetPassword_EmailSent ()
  , Msg_ResetPassword_ForgottenPassword ()
  , Msg_ResetPassword_EmailSubject ()
  , Msg_ResetPassword_EmailBody ()
  , Msg_ResetPassword_GenericError ()
  , Msg_ResetPassword_InvalidPassword ()

  , Msg_Profile_User ()
  , Msg_Profile_Email ()
  , Msg_Profile_FullName ()
  , Msg_Profile_Timezone ()
  , Msg_Profile_SaveButton ()
  , Msg_Profile_OldPassword ()
  , Msg_Profile_NewPassword ()
  , Msg_Profile_NewPasswordAgain ()
  , Msg_Profile_ChangePwdButton ()
  , Msg_Profile_Language ()
  , Msg_Profile_PasswordHasBeenChanged ()

  , Msg_SetUserPassword_NonRegisteredUser ()
  , Msg_SetUserPassword_User ()
  , Msg_SetUserPassword_NewPassword ()
  , Msg_SetUserPassword_NewPasswordAgain ()
  , Msg_SetUserPassword_SetButton ()

  , Msg_InputHandlers_BinEval ()
  , Msg_InputHandlers_PctEval ()
  , Msg_InputHandlers_Role_Student ()
  , Msg_InputHandlers_Role_GroupAdmin ()
  , Msg_InputHandlers_Role_CourseAdmin ()
  , Msg_InputHandlers_Role_Admin ()

  , Msg_Home_NewSolution ()
  , Msg_Home_AdminTasks ()
  , Msg_Home_CourseAdminTasks ()
  , Msg_Home_CourseAdministration_Info ()
  , Msg_Home_NoCoursesYet ()
  , Msg_Home_SubmissionTable_Info ()
  , Msg_Home_CourseAdministration ()
  , Msg_Home_CourseSubmissionTableList_Info ()
  , Msg_Home_GroupAdminTasks ()
  , Msg_Home_NoGroupsYet ()
  , Msg_Home_StudentTasks ()
  , Msg_Home_HasNoRegisteredCourses ()
  , Msg_Home_HasNoAssignments ()
  , Msg_Home_Assignments_Info ()
  , Msg_Home_Course ()
  , Msg_Home_CourseAdmin ()
  , Msg_Home_Assignment ()
  , Msg_Home_Deadline ()
  , Msg_Home_Evaluation ()
  , Msg_Home_ClosedSubmission ()
  , Msg_Home_SubmissionCell_NoSubmission ()
  , Msg_Home_SubmissionCell_NonEvaluated ()
  , Msg_Home_SubmissionCell_Accepted ()
  , Msg_Home_SubmissionCell_Rejected ()
  , Msg_Home_SubmissionCell_Tests_Passed ()
  , Msg_Home_SubmissionCell_Tests_Failed ()
  , Msg_Home_SubmissionTable_NoCoursesOrStudents ()
  , Msg_SubmissionList_NotEvaluatedYet ()
  , Msg_Home_SubmissionTable_StudentName ()
  , Msg_Home_SubmissionTable_Username ()
  , Msg_Home_SubmissionTable_Summary ()

  , Msg_Home_SubmissionTable_Accepted ()
  , Msg_Home_SubmissionTable_Rejected ()
  , Msg_Home_NonBinaryEvaluation ()
  , Msg_Home_HasNoSummary ()
  , Msg_Home_NonPercentageEvaluation ()
  , Msg_Home_DeleteUsersFromCourse ()
  , Msg_Home_DeleteUsersFromGroup ()
  , Msg_Home_NotAdministratedTestScripts ()
  , Msg_Home_NoTestScriptsWereDefined ()
  , Msg_Home_ModifyTestScriptTable ()
  , Msg_Home_CourseAssignmentIDPreffix ()
  , Msg_Home_GroupAssignmentIDPreffix ()
  , Msg_Home_ThereIsIsolatedAssignment ()

  , Msg_NewTestScript_Name ()
  , Msg_NewTestScript_Type ()
  , Msg_NewTestScript_Description ()
  , Msg_NewTestScript_Notes ()
  , Msg_NewTestScript_Script ()
  , Msg_NewTestScript_Save ()
  , Msg_NewTestScript_Course ()
  , Msg_NewTestScript_HasNoCourses ()
  , Msg_NewTestScript_ScriptTypeHelp ()

  , Msg_UploadFile_FileSelection ()
  , Msg_UploadFile_Directory ()
  , Msg_UploadFile_Info ()
  , Msg_UploadFile_UploadButton ()
  , Msg_UploadFile_FileName ()
  , Msg_UploadFile_FileSize ()
  , Msg_UploadFile_FileDate ()
  , Msg_UploadFile_Successful ()
  , Msg_UploadFile_NoFileReceived ()
  , Msg_UploadFile_PolicyFailure ()
  , Msg_UploadFile_UnnamedFile ()
  , Msg_UploadFile_InternalError ()
  , Msg_UploadFile_ErrorInManyUploads ()

  , Msg_UserStory_SetTimeZone ()
  , Msg_UserStory_ChangedUserDetails ()
  , Msg_UserStory_CreateCourse ()
  , Msg_UserStory_SetCourseAdmin ()
  , Msg_UserStory_SetGroupAdmin ()
  , Msg_UserStory_CreateGroup ()
  , Msg_UserStory_SubscribedToGroup ()
  , Msg_UserStory_SubscribedToGroup_ChangeNotAllowed ()
  , Msg_UserStory_NewGroupAssignment ()
  , Msg_UserStory_NewCourseAssignment ()
  , Msg_UserStory_UsersAreDeletedFromCourse ()
  , Msg_UserStory_UsersAreDeletedFromGroup ()
  , Msg_UserStory_SuccessfulCourseUnsubscription ()
  , Msg_UserStory_NewTestScriptIsCreated ()
  , Msg_UserStory_ModifyTestScriptIsDone ()
  , Msg_UserStory_EvalTypeWarning ()

  , Msg_UserStoryError_UnknownError ()
  , Msg_UserStoryError_Message ()
  , Msg_UserStoryError_SameUserIsLoggedIn ()
  , Msg_UserStoryError_InvalidUsernameOrPassword ()
  , Msg_UserStoryError_NoCourseAdminOfCourse ()
  , Msg_UserStoryError_NoAssociatedTestScript ()
  , Msg_UserStoryError_NoGroupAdmin ()
  , Msg_UserStoryError_NoGroupAdminOfGroup ()
  , Msg_UserStoryError_AlreadyHasSubmission ()
  , Msg_UserStoryError_UserIsNotLoggedIn ()
  , Msg_UserStoryError_RegistrationProcessError ()
  , Msg_UserStoryError_AuthenticationNeeded ()
  , Msg_UserStoryError_SubmissionDeadlineIsReached ()
  , Msg_UserStoryError_XID ()
  , Msg_UserStoryError_TestAgentError ()
  , Msg_UserStoryError_EmptyAssignmentTitle ()
  , Msg_UserStoryError_EmptyAssignmentDescription ()

  , Msg_UserStoryError_NonAdministratedCourse ()
  , Msg_UserStoryError_NonAdministratedGroup ()
  , Msg_UserStoryError_NonAdministratedAssignment ()
  , Msg_UserStoryError_NonRelatedAssignment ()
  , Msg_UserStoryError_NonAdministratedSubmission ()
  , Msg_UserStoryError_NonAdministratedTestScript ()
  , Msg_UserStoryError_NonCommentableSubmission ()
  , Msg_UserStoryError_NonAccessibleSubmission ()

  , Msg_UserActions_ChangedUserDetails ()

  , Msg_LinkText_Login ()
  , Msg_LinkText_Logout ()
  , Msg_LinkText_Home ()
  , Msg_LinkText_Profile ()
  , Msg_LinkText_Error ()
  , Msg_LinkText_CourseAdministration ()
  , Msg_LinkText_CourseOverview ()
  , Msg_LinkText_Submission ()
  , Msg_LinkText_SubmissionList ()
  , Msg_LinkText_UserSubmissions ()
  , Msg_LinkText_NewTestScript ()
  , Msg_LinkText_ModifyTestScript ()
  , Msg_LinkText_UploadFile ()
  , Msg_LinkText_ModifyEvaluation ()
  , Msg_LinkText_ViewAssignment ()
  , Msg_LinkText_SubmissionDetails ()
  , Msg_LinkText_Administration ()
  , Msg_LinkText_Evaluation ()
  , Msg_LinkText_EvaluationTable ()
  , Msg_LinkText_GroupRegistration ()
  , Msg_LinkText_CreateCourse ()
  , Msg_LinkText_UserDetails ()
  , Msg_LinkText_AssignCourseAdmin ()
  , Msg_LinkText_CreateGroup ()
  , Msg_LinkText_AssignGroupAdmin ()
  , Msg_LinkText_NewGroupAssignment ()
  , Msg_LinkText_NewCourseAssignment ()
  , Msg_LinkText_ModifyAssignment ()
  , Msg_LinkText_NewGroupAssignmentPreview ()
  , Msg_LinkText_NewCourseAssignmentPreview ()
  , Msg_LinkText_ModifyAssignmentPreview ()
  , Msg_LinkText_ChangePassword ()
  , Msg_LinkText_SetUserPassword ()
  , Msg_LinkText_DeleteUsersFromCourse ()
  , Msg_LinkText_DeleteUsersFromGroup ()
  , Msg_LinkText_UnsubscribeFromCourse ()
  , Msg_LinkText_GetSubmission ()

  , Msg_TestScriptTypeSimple ()
  , Msg_TestScriptTypeZipped ()

  , Msg_Domain_EvalPassed ()
  , Msg_Domain_EvalFailed ()
  , Msg_Domain_EvalNoResultError ()
  , Msg_Domain_EvalPercentage ()

  , Msg_SeeMore_SeeMore ()
  , Msg_SeeMore_SeeLess ()

  ]

translationMaxIndex = (length translationList) - 1

translationToMap = Map.fromList $ zip [0..] translationList
translationFromMap = Map.fromList $ zip translationList [0..]

trToEnum n = maybe
  (error (concat ["There is no Translation key for the ", show n]))
  (id)
  (Map.lookup n translationToMap)

trFromEnum n = maybe
  (error (concat ["There is no int for the ", show n]))
  (id)
  (Map.lookup n translationFromMap)

instance Bounded (Translation ()) where
  minBound = trToEnum 0
  maxBound = trToEnum translationMaxIndex

instance Enum (Translation ()) where
  toEnum = trToEnum
  fromEnum = trFromEnum
