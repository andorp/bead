module Bead.View.Translation where

import Text.Printf (printf)

-- Translation is an enumeration all the possbile messages that could be rendered
-- on every page with the associated value.
newtype Translation a = T (Int,a)
  deriving (Show,Read,Ord,Eq)

trans :: Translation a -> a
trans (T (_,x)) = x

t :: Int -> a -> Translation a
t = curry T

msg_Login_PageTitle             = t 0x00010000
msg_Login_Username              = t 0x00010001
msg_Login_Password              = t 0x00010002
msg_Login_Submit                = t 0x00010003
msg_Login_Title                 = t 0x00010004
msg_Login_Registration          = t 0x00010005
msg_Login_Forgotten_Password    = t 0x00010006
msg_Login_InternalError         = t 0x00010007
msg_Login_SelectLanguage        = t 0x00010008
msg_Login_InvalidPasswordOrUser = t 0x00010009

msg_Routing_InvalidRoute    = t 0x00020000
msg_Routing_SessionTimedOut = t 0x00020001

msg_ErrorPage_Title         = t 0x00030000
msg_ErrorPage_GoBackToLogin = t 0x00030001
msg_ErrorPage_Header        = t 0x00030002

msg_Input_Group_Name         = t 0x00040000
msg_Input_Group_Description  = t 0x00040001
msg_Input_Group_Evaluation   = t 0x00040002
msg_Input_Course_Name        = t 0x00040003
msg_Input_Course_Description = t 0x00040004
msg_Input_Course_Evaluation  = t 0x00040005
msg_Input_Course_TestScript  = t 0x00040006
msg_Input_User_Role          = t 0x00040007
msg_Input_User_Email         = t 0x00040008
msg_Input_User_FullName      = t 0x00040009
msg_Input_User_TimeZone      = t 0x0004000A
msg_Input_User_Language      = t 0x0004000B
msg_Input_TestScriptSimple   = t 0x0004000C
msg_Input_TestScriptZipped   = t 0x0004000D

msg_CourseAdmin_CreateCourse       = t 0x00050000
msg_CourseAdmin_AssignAdmin        = t 0x00050001
msg_CourseAdmin_AssignAdmin_Button = t 0x00050002
msg_CourseAdmin_CreateGroup        = t 0x00050003
msg_CourseAdmin_NoCourses          = t 0x00050004
msg_CourseAdmin_Course             = t 0x00050005
msg_CourseAdmin_PctHelpMessage     = t 0x00050006
msg_CourseAdmin_NoGroups           = t 0x00050007
msg_CourseAdmin_NoGroupAdmins      = t 0x00050008
msg_CourseAdmin_Group              = t 0x00050009
msg_CourseAdmin_Admin              = t 0x0005000A
msg_CourseAdmin_GroupAdmins_Info   = t 0x0005000B
msg_CourseAdmin_GroupAdmins_Group  = t 0x0005000C
msg_CourseAdmin_GroupAdmins_Admins = t 0x0005000D

msg_Administration_NewCourse               = t 0x00060000
msg_Administration_PctHelpMessage          = t 0x00060001
msg_Administration_CreatedCourses          = t 0x00060002
msg_Administration_CreateCourse            = t 0x00060003
msg_Administration_AssignCourseAdminTitle  = t 0x00060004
msg_Administration_NoCourses               = t 0x00060005
msg_Administration_NoCourseAdmins          = t 0x00060006
msg_Administration_AssignCourseAdminButton = t 0x00060007
msg_Administration_ChangeUserProfile       = t 0x00060008
msg_Administration_SelectUser              = t 0x00060009
msg_Administration_HowToAddMoreAdmins      = t 0x0006000A
msg_Administration_CourseAdmins_Info       = t 0x0006000B
msg_Administration_CourseAdmins_Course     = t 0x0006000C
msg_Administration_CourseAdmins_Admins     = t 0x0006000D

msg_NewAssignment_Title                = t 0x00070000
msg_NewAssignment_Title_Default        = t 0x00070001
msg_NewAssignment_SubmissionDeadline   = t 0x00070002
msg_NewAssignment_StartDate            = t 0x00070003
msg_NewAssignment_EndDate              = t 0x00070004
msg_NewAssignment_Description          = t 0x00070005
msg_NewAssignment_Description_Default  = t 0x00070006
msg_NewAssignment_Markdown             = t 0x00070007
msg_NewAssignment_CanBeUsed            = t 0x00070008
msg_NewAssignment_Properties           = t 0x00070009
msg_NewAssignment_Course               = t 0x0007000A
msg_NewAssignment_Group                = t 0x0007000B
msg_NewAssignment_SaveButton           = t 0x0007000C
msg_NewAssignment_PreviewButton        = t 0x0007000D
msg_NewAssignment_Title_Normal         = t 0x0007000E
msg_NewAssignment_Title_BallotBox      = t 0x0007000F
msg_NewAssignment_Title_Password       = t 0x00070010
msg_NewAssignment_Info_Normal          = t 0x00070011
msg_NewAssignment_Info_BallotBox       = t 0x00070012
msg_NewAssignment_Info_Password        = t 0x00070013
msg_NewAssignment_TestCase             = t 0x00070014
msg_NewAssignment_TestScripts          = t 0x00070015
msg_NewAssignment_DoNotOverwrite       = t 0x00070016
msg_NewAssignment_NoTesting            = t 0x00070017
msg_NewAssignment_TestFile             = t 0x00070018
msg_NewAssignment_TestFile_Info        = t 0x00070019
msg_NewAssignment_AssignmentPreview    = t 0x0007001A
msg_NewAssignment_BallotBox            = t 0x0007001B
msg_NewAssignment_PasswordProtected    = t 0x0007001C
msg_NewAssignment_Password             = t 0x0007001D
msg_NewAssignment_EvaluationType       = t 0x0007001E
msg_NewAssignment_BinaryEvaluation     = t 0x0007001F
msg_NewAssignment_PercentageEvaluation = t 0x00070020
msg_NewAssignment_SubmissionType       = t 0x00070021
msg_NewAssignment_TextSubmission       = t 0x00070022
msg_NewAssignment_ZipSubmission        = t 0x00070023
msg_NewAssignment_EvalTypeWarn         = t 0x00070024
msg_NewAssignment_Isolated             = t 0x00070025
msg_NewAssignment_Info_Isolated        = t 0x00070026
msg_NewAssignment_Info_NoOfTries       = t 0x00070027
msg_NewAssignment_NoOfTries            = t 0x00070028

msg_GroupRegistration_RegisteredCourses         = t 0x00080000
msg_GroupRegistration_SelectGroup               = t 0x00080001
msg_GroupRegistration_NoRegisteredCourses       = t 0x00080002
msg_GroupRegistration_Courses                   = t 0x00080003
msg_GroupRegistration_Admins                    = t 0x00080004
msg_GroupRegistration_NoAvailableCourses        = t 0x00080005
msg_GroupRegistration_Register                  = t 0x00080006
msg_GroupRegistration_Unsubscribe               = t 0x00080007
msg_GroupRegistration_NoUnsubscriptionAvailable = t 0x00080008
msg_GroupRegistration_Warning                   = t 0x00080009

msg_UserDetails_SaveButton      = t 0x00090000
msg_UserDetails_NonExistingUser = t 0x00090001

msg_Submission_Course              = t 0x000A0000
msg_Submission_Admin               = t 0x000A0001
msg_Submission_Assignment          = t 0x000A0002
msg_Submission_Deadline            = t 0x000A0003
msg_Submission_Description         = t 0x000A0004
msg_Submission_Solution            = t 0x000A0005
msg_Submission_Submit              = t 0x000A0006
msg_Submission_TimeLeft            = t 0x000A0007
msg_Submission_Days                = t 0x000A0008
msg_Submission_DeadlineReached     = t 0x000A0009
msg_Submission_InvalidPassword     = t 0x000A000A
msg_Submission_NonUsersAssignment  = t 0x000A000B
msg_Submission_Password            = t 0x000A000C
msg_Submission_PasswordAgain       = t 0x000A000D
msg_Submission_Info_Password       = t 0x000A000E
msg_Submission_Info_File           = t 0x000A000F
msg_Submission_File_NoFileReceived = t 0x000A0010
msg_Submission_File_PolicyFailure  = t 0x000A0011
msg_Submission_File_InvalidFile    = t 0x000A0012
msg_Submission_File_InternalError  = t 0x000A0013
msg_Submission_Remaining           = t 0x000A0014
msg_Submission_NoTriesLeft         = t 0x000A0015

msg_Comments_Title                    = t 0x000B0000
msg_Comments_SubmitButton             = t 0x000B0001
msg_Comments_AuthorTestScript_Public  = t 0x000B0002
msg_Comments_AuthorTestScript_Private = t 0x000B0003
msg_Comments_TestPassed               = t 0x000B0004
msg_Comments_TestFailed               = t 0x000B0005
msg_Comments_BinaryResultPassed       = t 0x000B0006
msg_Comments_BinaryResultFailed       = t 0x000B0007
msg_Comments_PercentageResult         = t 0x000B0008

msg_Evaluation_Title                        = t 0x000C0000
msg_Evaluation_Course                       = t 0x000C0001
msg_Evaluation_Group                        = t 0x000C0002
msg_Evaluation_Student                      = t 0x000C0003
msg_Evaluation_SaveButton                   = t 0x000C0004
msg_Evaluation_Submitted_Solution           = t 0x000C0005
msg_Evaluation_Submitted_Solution_Text_Info = t 0x000C0006
msg_Evaluation_Submitted_Solution_Text_Link = t 0x000C0007
msg_Evaluation_Submitted_Solution_Zip_Info  = t 0x000C0008
msg_Evaluation_Submitted_Solution_Zip_Link  = t 0x000C0009
msg_Evaluation_Accepted                     = t 0x000C000A
msg_Evaluation_Rejected                     = t 0x000C000B
msg_Evaluation_New_Comment                  = t 0x000C000C
msg_Evaluation_Info                         = t 0x000C000D
msg_Evaluation_Username                     = t 0x000C000E
msg_Evaluation_SubmissionDate               = t 0x000C000F

msg_SubmissionDetails_Course             = t 0x000D0000
msg_SubmissionDetails_Admins             = t 0x000D0001
msg_SubmissionDetails_Assignment         = t 0x000D0002
msg_SubmissionDetails_Deadline           = t 0x000D0003
msg_SubmissionDetails_Description        = t 0x000D0004
msg_SubmissionDetails_Solution           = t 0x000D0005
msg_SubmissionDetails_Solution_Text_Info = t 0x000D0006
msg_SubmissionDetails_Solution_Text_Link = t 0x000D0007
msg_SubmissionDetails_Solution_Zip_Info  = t 0x000D0008
msg_SubmissionDetails_Solution_Zip_Link  = t 0x000D0009
msg_SubmissionDetails_Evaluation         = t 0x000D000A
msg_SubmissionDetails_NewComment         = t 0x000D000B
msg_SubmissionDetails_SubmitComment      = t 0x000D000C
msg_SubmissionDetails_InvalidSubmission  = t 0x000D000D

msg_Registration_Title                     = t 0x000E0000
msg_Registration_Username                  = t 0x000E0001
msg_Registration_Email                     = t 0x000E0002
msg_Registration_FullName                  = t 0x000E0003
msg_Registration_SubmitButton              = t 0x000E0004
msg_Registration_GoBackToLogin             = t 0x000E0005
msg_Registration_InvalidUsername           = t 0x000E0006
msg_Registration_HasNoUserAccess           = t 0x000E0007
msg_Registration_UserAlreadyExists         = t 0x000E0008
msg_Registration_RegistrationNotSaved      = t 0x000E0009
msg_Registration_EmailSubject              = t 0x000E000A
msg_Registration_EmailBody                 = t 0x000E000B
msg_Registration_RequestParameterIsMissing = t 0x000E000C

msg_RegistrationFinalize_NoRegistrationParametersAreFound = t 0x000F0000
msg_RegistrationFinalize_SomeError                        = t 0x000F0001
msg_RegistrationFinalize_InvalidToken                     = t 0x000F0002
msg_RegistrationFinalize_UserAlreadyExist                 = t 0x000F0003
msg_RegistrationFinalize_Password                         = t 0x000F0004
msg_RegistrationFinalize_PwdAgain                         = t 0x000F0005
msg_RegistrationFinalize_Timezone                         = t 0x000F0006
msg_RegistrationFinalize_SubmitButton                     = t 0x000F0007
msg_RegistrationFinalize_GoBackToLogin                    = t 0x000F0008

msg_RegistrationCreateStudent_NoParameters  = t 0x00100000
msg_RegistrationCreateStudent_InternalError = t 0x00100001
msg_RegistrationCreateStudent_InvalidToken  = t 0x00100002

msg_RegistrationTokenSend_Title         = t 0x00110000
msg_RegistrationTokenSend_StoryFailed   = t 0x00110001
msg_RegistrationTokenSend_GoBackToLogin = t 0x00110002

msg_EvaluationTable_EmptyUnevaluatedSolutions = t 0x00120000
msg_EvaluationTable_Course                    = t 0x00120001
msg_EvaluationTable_Group                     = t 0x00120002
msg_EvaluationTable_Student                   = t 0x00120003
msg_EvaluationTable_Assignment                = t 0x00120004
msg_EvaluationTable_Link                      = t 0x00120005
msg_EvaluationTable_Solution                  = t 0x00120006
msg_EvaluationTable_Info                      = t 0x00120007
msg_EvaluationTable_CourseAssignment          = t 0x00120008
msg_EvaluationTable_GroupAssignment           = t 0x00120009
msg_EvaluationTable_MiscCourseAssignment      = t 0x0012000A
msg_EvaluationTable_CourseAssignmentInfo      = t 0x0012000B
msg_EvaluationTable_GroupAssignmentInfo       = t 0x0012000C
msg_EvaluationTable_MiscCourseAssignmentInfo  = t 0x0012000D
msg_EvaluationTable_Username                  = t 0x0012000E
msg_EvaluationTable_DateOfSubmission          = t 0x0012000F

msg_UserSubmissions_NonAccessibleSubmissions = t 0x00130000
msg_UserSubmissions_Course                   = t 0x00130001
msg_UserSubmissions_Assignment               = t 0x00130002
msg_UserSubmissions_Student                  = t 0x00130003
msg_UserSubmissions_SubmittedSolutions       = t 0x00130004
msg_UserSubmissions_SubmissionDate           = t 0x00130005
msg_UserSubmissions_Evaluation               = t 0x00130006

msg_UserSubmissions_Accepted     = t 0x00140000
msg_UserSubmissions_Rejected     = t 0x00140001
msg_UserSubmissions_NotFound     = t 0x00140002
msg_UserSubmissions_NonEvaluated = t 0x00140003
msg_UserSubmissions_Tests_Passed = t 0x00140004
msg_UserSubmissions_Tests_Failed = t 0x00140005

msg_SubmissionList_CourseOrGroup           = t 0x00150000
msg_SubmissionList_Admin                   = t 0x00150001
msg_SubmissionList_Assignment              = t 0x00150002
msg_SubmissionList_Deadline                = t 0x00150003
msg_SubmissionList_Description             = t 0x00150004
msg_SubmissionList_SubmittedSolutions      = t 0x00150005
msg_SubmissionList_NotEvaluatedYet         = t 0x00150006
msg_SubmissionList_NoSubmittedSolutions    = t 0x00150007
msg_SubmissionList_NonAssociatedAssignment = t 0x00150008
msg_SubmissionList_NonReachableAssignment  = t 0x00150009
msg_SubmissionList_Info                    = t 0x0015000A
msg_SubmissionList_NotFound                = t 0x0015000B
msg_SubmissionList_TestsPassed             = t 0x0015000C
msg_SubmissionList_TestsFailed             = t 0x0015000D
msg_SubmissionList_Passed                  = t 0x0015000E
msg_SubmissionList_Failed                  = t 0x0015000F

msg_ResetPassword_UserDoesNotExist  = t 0x00160000
msg_ResetPassword_PasswordIsSet     = t 0x00160001
msg_ResetPassword_GoBackToLogin     = t 0x00160002
msg_ResetPassword_Username          = t 0x00160003
msg_ResetPassword_Email             = t 0x00160004
msg_ResetPassword_NewPwdButton      = t 0x00160005
msg_ResetPassword_EmailSent         = t 0x00160006
msg_ResetPassword_ForgottenPassword = t 0x00160007
msg_ResetPassword_EmailSubject      = t 0x00160008
msg_ResetPassword_EmailBody         = t 0x00160009
msg_ResetPassword_GenericError      = t 0x0016000A
msg_ResetPassword_InvalidPassword   = t 0x0016000B

msg_Profile_User                   = t 0x00170000
msg_Profile_Email                  = t 0x00170001
msg_Profile_FullName               = t 0x00170002
msg_Profile_Timezone               = t 0x00170003
msg_Profile_SaveButton             = t 0x00170004
msg_Profile_OldPassword            = t 0x00170005
msg_Profile_NewPassword            = t 0x00170006
msg_Profile_NewPasswordAgain       = t 0x00170007
msg_Profile_ChangePwdButton        = t 0x00170008
msg_Profile_Language               = t 0x00170009
msg_Profile_PasswordHasBeenChanged = t 0x0017000A

msg_SetUserPassword_NonRegisteredUser = t 0x00180000
msg_SetUserPassword_User              = t 0x00180001
msg_SetUserPassword_NewPassword       = t 0x00180002
msg_SetUserPassword_NewPasswordAgain  = t 0x00180003
msg_SetUserPassword_SetButton         = t 0x00180004

msg_InputHandlers_BinEval          = t 0x00190000
msg_InputHandlers_PctEval          = t 0x00190001
msg_InputHandlers_Role_Student     = t 0x00190002
msg_InputHandlers_Role_GroupAdmin  = t 0x00190003
msg_InputHandlers_Role_CourseAdmin = t 0x00190004
msg_InputHandlers_Role_Admin       = t 0x00190005

msg_Home_NewSolution                         = t 0x001A0000
msg_Home_AdminTasks                          = t 0x001A0001
msg_Home_CourseAdminTasks                    = t 0x001A0002
msg_Home_CourseAdministration_Info           = t 0x001A0003
msg_Home_NoCoursesYet                        = t 0x001A0004
msg_Home_GroupAdminTasks                     = t 0x001A0005
msg_Home_NoGroupsYet                         = t 0x001A0006
msg_Home_SubmissionTable_Info                = t 0x001A0007
msg_Home_CourseAdministration                = t 0x001A0008
msg_Home_CourseSubmissionTableList_Info      = t 0x001A0009
msg_Home_StudentTasks                        = t 0x001A000A
msg_Home_HasNoRegisteredCourses              = t 0x001A000B
msg_Home_HasNoAssignments                    = t 0x001A000C
msg_Home_Assignments_Info                    = t 0x001A000D
msg_Home_Course                              = t 0x001A000E
msg_Home_Limit                               = t 0x001A000F
msg_Home_CourseAdmin                         = t 0x001A0010
msg_Home_Assignment                          = t 0x001A0011
msg_Home_Deadline                            = t 0x001A0012
msg_Home_Evaluation                          = t 0x001A0013
msg_Home_ClosedSubmission                    = t 0x001A0014
msg_Home_SubmissionCell_NoSubmission         = t 0x001A0015
msg_Home_SubmissionCell_NonEvaluated         = t 0x001A0016
msg_Home_SubmissionCell_Accepted             = t 0x001A0017
msg_Home_SubmissionCell_Rejected             = t 0x001A0018
msg_Home_SubmissionCell_Tests_Failed         = t 0x001A0019
msg_Home_SubmissionCell_Tests_Passed         = t 0x001A001A
msg_Home_SubmissionTable_NoCoursesOrStudents = t 0x001A001B
msg_Home_Remains                             = t 0x001A001C
msg_Home_Reached                             = t 0x001A001D

msg_Home_SubmissionTable_StudentName = t 0x001B0000
msg_Home_SubmissionTable_Username    = t 0x001B0001
msg_Home_SubmissionTable_Summary     = t 0x001B0002

msg_Home_SubmissionTable_Accepted    = t 0x001C0000
msg_Home_SubmissionTable_Rejected    = t 0x001C0001
msg_Home_NonBinaryEvaluation         = t 0x001C0002
msg_Home_HasNoSummary                = t 0x001C0003
msg_Home_NonPercentageEvaluation     = t 0x001C0004
msg_Home_DeleteUsersFromCourse       = t 0x001C0005
msg_Home_DeleteUsersFromGroup        = t 0x001C0006
msg_Home_NotAdministratedTestScripts = t 0x001C0007
msg_Home_NoTestScriptsWereDefined    = t 0x001C0008
msg_Home_ModifyTestScriptTable       = t 0x001C0009
msg_Home_CourseAssignmentIDPreffix   = t 0x001C000A
msg_Home_GroupAssignmentIDPreffix    = t 0x001C000B
msg_Home_ThereIsIsolatedAssignment   = t 0x001C000C

msg_NewTestScript_Name           = t 0x001D0000
msg_NewTestScript_Type           = t 0x001D0001
msg_NewTestScript_Description    = t 0x001D0002
msg_NewTestScript_Notes          = t 0x001D0003
msg_NewTestScript_Script         = t 0x001D0004
msg_NewTestScript_Save           = t 0x001D0005
msg_NewTestScript_Course         = t 0x001D0006
msg_NewTestScript_HasNoCourses   = t 0x001D0007
msg_NewTestScript_ScriptTypeHelp = t 0x001D0008

msg_UploadFile_FileSelection      = t 0x001E0000
msg_UploadFile_Directory          = t 0x001E0001
msg_UploadFile_Info               = t 0x001E0002
msg_UploadFile_UploadButton       = t 0x001E0003
msg_UploadFile_FileName           = t 0x001E0004
msg_UploadFile_FileSize           = t 0x001E0005
msg_UploadFile_FileDate           = t 0x001E0006
msg_UploadFile_Successful         = t 0x001E0007
msg_UploadFile_NoFileReceived     = t 0x001E0008
msg_UploadFile_PolicyFailure      = t 0x001E0009
msg_UploadFile_UnnamedFile        = t 0x001E000A
msg_UploadFile_InternalError      = t 0x001E000B
msg_UploadFile_ErrorInManyUploads = t 0x001E000C

msg_UserStory_SetTimeZone                        = t 0x001F0000
msg_UserStory_ChangedUserDetails                 = t 0x001F0001
msg_UserStory_CreateCourse                       = t 0x001F0002
msg_UserStory_SetCourseAdmin                     = t 0x001F0003
msg_UserStory_SetGroupAdmin                      = t 0x001F0004
msg_UserStory_CreateGroup                        = t 0x001F0005
msg_UserStory_SubscribedToGroup                  = t 0x001F0006
msg_UserStory_SubscribedToGroup_ChangeNotAllowed = t 0x001F0007
msg_UserStory_NewGroupAssignment                 = t 0x001F0008
msg_UserStory_NewCourseAssignment                = t 0x001F0009
msg_UserStory_UsersAreDeletedFromCourse          = t 0x001F000A
msg_UserStory_UsersAreDeletedFromGroup           = t 0x001F000B
msg_UserStory_SuccessfulCourseUnsubscription     = t 0x001F000C
msg_UserStory_NewTestScriptIsCreated             = t 0x001F000D
msg_UserStory_ModifyTestScriptIsDone             = t 0x001F000E
msg_UserStory_AlreadyEvaluated                   = t 0x001F000F
msg_UserStory_EvalTypeWarning                    = t 0x001F0010

msg_UserStoryError_UnknownError                = t 0x00200000
msg_UserStoryError_Message                     = t 0x00200001
msg_UserStoryError_SameUserIsLoggedIn          = t 0x00200002
msg_UserStoryError_InvalidUsernameOrPassword   = t 0x00200003
msg_UserStoryError_NoCourseAdminOfCourse       = t 0x00200004
msg_UserStoryError_NoAssociatedTestScript      = t 0x00200005
msg_UserStoryError_NoGroupAdmin                = t 0x00200006
msg_UserStoryError_NoGroupAdminOfGroup         = t 0x00200007
msg_UserStoryError_AlreadyHasSubmission        = t 0x00200008
msg_UserStoryError_UserIsNotLoggedIn           = t 0x00200009
msg_UserStoryError_RegistrationProcessError    = t 0x0020000A
msg_UserStoryError_AuthenticationNeeded        = t 0x0020000B
msg_UserStoryError_SubmissionDeadlineIsReached = t 0x0020000C
msg_UserStoryError_XID                         = t 0x0020000D
msg_UserStoryError_TestAgentError              = t 0x0020000E
msg_UserStoryError_EmptyAssignmentTitle        = t 0x0020000F
msg_UserStoryError_EmptyAssignmentDescription  = t 0x00200010

msg_UserStoryError_NonAdministratedCourse     = t 0x00210000
msg_UserStoryError_NonAdministratedGroup      = t 0x00210001
msg_UserStoryError_NonAdministratedAssignment = t 0x00210002
msg_UserStoryError_NonRelatedAssignment       = t 0x00210003
msg_UserStoryError_NonAdministratedSubmission = t 0x00210004
msg_UserStoryError_NonAdministratedTestScript = t 0x00210005
msg_UserStoryError_NonCommentableSubmission   = t 0x00210006
msg_UserStoryError_NonAccessibleSubmission    = t 0x00210007
msg_UserStoryError_BlockedSubmission          = t 0x00210008

msg_UserActions_ChangedUserDetails = t 0x00220000

msg_LinkText_Login                      = t 0x00230000
msg_LinkText_Logout                     = t 0x00230001
msg_LinkText_Home                       = t 0x00230002
msg_LinkText_Profile                    = t 0x00230003
msg_LinkText_Error                      = t 0x00230004
msg_LinkText_CourseAdministration       = t 0x00230005
msg_LinkText_CourseOverview             = t 0x00230006
msg_LinkText_Submission                 = t 0x00230007
msg_LinkText_SubmissionList             = t 0x00230008
msg_LinkText_UserSubmissions            = t 0x00230009
msg_LinkText_NewTestScript              = t 0x0023000A
msg_LinkText_ModifyTestScript           = t 0x0023000B
msg_LinkText_UploadFile                 = t 0x0023000C
msg_LinkText_ModifyEvaluation           = t 0x0023000D
msg_LinkText_ViewAssignment             = t 0x0023000E
msg_LinkText_SubmissionDetails          = t 0x0023000F
msg_LinkText_Administration             = t 0x00230010
msg_LinkText_Evaluation                 = t 0x00230011
msg_LinkText_EvaluationTable            = t 0x00230012
msg_LinkText_GroupRegistration          = t 0x00230013
msg_LinkText_CreateCourse               = t 0x00230014
msg_LinkText_UserDetails                = t 0x00230015
msg_LinkText_AssignCourseAdmin          = t 0x00230016
msg_LinkText_CreateGroup                = t 0x00230017
msg_LinkText_AssignGroupAdmin           = t 0x00230018
msg_LinkText_NewGroupAssignment         = t 0x00230019
msg_LinkText_NewCourseAssignment        = t 0x0023001A
msg_LinkText_ModifyAssignment           = t 0x0023001B
msg_LinkText_NewGroupAssignmentPreview  = t 0x0023001C
msg_LinkText_NewCourseAssignmentPreview = t 0x0023001D
msg_LinkText_ModifyAssignmentPreview    = t 0x0023001E
msg_LinkText_ChangePassword             = t 0x0023001F
msg_LinkText_SetUserPassword            = t 0x00230020
msg_LinkText_DeleteUsersFromCourse      = t 0x00230021
msg_LinkText_DeleteUsersFromGroup       = t 0x00230022
msg_LinkText_UnsubscribeFromCourse      = t 0x00230023
msg_LinkText_GetSubmission              = t 0x00230024

msg_TestScriptTypeSimple = t 0x00240000
msg_TestScriptTypeZipped = t 0x00240001

msg_Domain_EvalPassed        = t 0x00250000
msg_Domain_EvalFailed        = t 0x00250001
msg_Domain_EvalNoResultError = t 0x00250002
msg_Domain_EvalPercentage    = t 0x00250003

msg_SeeMore_SeeMore = t 0x00260000
msg_SeeMore_SeeLess = t 0x00260001

-- The I18N is a mapping from a given translation key
-- to the actual translation of the message
type I18N = Translation String -> String

-- | The Translation Message represents a message that
-- can be rendered out the the UI, the message could
-- be a normal message or a parametrized one
data TransMsg
  = TransMsg (Translation String)
  | TransPrmMsg (Translation String) String
  | TransPrm2Msg (Translation String) String String
  | TransPrm3Msg (Translation String) String String String
  deriving (Show)

-- Template method for TransMsg function
transMsgCata
  transMsg     f
  transPrmMsg  g
  transPrm2Msg h
  transPrm3Msg i
  tm = case tm of
    TransMsg     t          -> transMsg     (f t)
    TransPrmMsg  t p1       -> transPrmMsg  (g t) p1
    TransPrm2Msg t p1 p2    -> transPrm2Msg (h t) p1 p2
    TransPrm3Msg t p1 p2 p3 -> transPrm3Msg (i t) p1 p2 p3

-- Translate the parametrized message with the given localization
translateMessage :: I18N -> TransMsg -> String
translateMessage i18n = transMsgCata
  id     i18n
  printf i18n
  printf i18n
  printf i18n
