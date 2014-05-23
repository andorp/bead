{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.SubmissionTable (
    AdministratedCourses
  , AdministratedGroups
  , CourseTestScriptInfos
  , SubmissionTableContext(..)
  , submissionTable
  , submissionTableContext
  , sortUserLines
  , coloredSubmissionCell
  ) where

import           Data.Char (isAlphaNum)
import           Data.Function
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.String
import           Data.Time
import           Numeric

import qualified Bead.Domain.Entities as E
import           Bead.Domain.Relationships
import           Bead.Domain.Evaluation
import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (UserStory)
import qualified Bead.Controller.UserStories as S
import           Bead.View.Snap.Content
import qualified Bead.View.Snap.DataBridge as Param

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


type AdministratedCourses = Map CourseKey E.Course

type AdministratedGroups  = Map GroupKey  (E.Group, String)

type CourseTestScriptInfos = Map CourseKey [(TestScriptKey, TestScriptInfo)]

data SubmissionTableContext = SubmissionTableContext {
    stcAdminCourses :: AdministratedCourses
  , stcAdminGroups  :: AdministratedGroups
  , stcCourseTestScriptInfos :: CourseTestScriptInfos
  }

submissionTableContextCata f (SubmissionTableContext courses groups testscripts)
  = f courses groups testscripts

submissionTableContext :: UserStory SubmissionTableContext
submissionTableContext = do
  ac <- S.administratedCourses
  ag <- S.administratedGroups
  ts <- Map.fromList <$> mapM (testScriptForCourse . fst) ac
  return $! SubmissionTableContext {
      stcAdminCourses = adminCourseMap ac
    , stcAdminGroups  = adminGroupMap ag
    , stcCourseTestScriptInfos = ts
    }
  where
    testScriptForCourse ck = do
      infos <- S.testScriptInfos ck
      return (ck, infos)

    adminCourseMap = Map.fromList

    adminGroupMap = Map.fromList . map (\(k,g,c) -> (k,(g,c)))

submissionTable :: String -> UTCTime -> SubmissionTableContext -> SubmissionTableInfo -> IHtml
submissionTable tableId now stb table = submissionTableContextCata html stb where
  html courses groups testscripts = do
    msg <- getI18N
    return $ do
      i18n msg $ submissionTablePart tableId now stb table
      i18n msg $ courseTestScriptTable testscripts table
      i18n msg $ assignmentCreationMenu courses groups table

-- Produces the HTML table from the submission table information,
-- if there is no users registered and submission posted to the
-- group or course students, an informational text is shown.
-- Supposing that the given tableid is unique name on the page.
submissionTablePart :: String -> UTCTime -> SubmissionTableContext -> SubmissionTableInfo -> IHtml

-- Empty table
submissionTablePart tableId _now _ctx s
  | and [null $ submissionTableInfoAssignments s, null $ stiUsers s] = do
    msg <- getI18N
    return $ do
      table tableId (className groupSubmissionTable) # informationalTable $ do
        headLine (stiCourse s)
        dataCell noStyle (fromString $ msg $ Msg_Home_SubmissionTable_NoCoursesOrStudents "There are no assignments or students yet.")

-- Non empty table
submissionTablePart tableId now ctx s = do
  msg <- getI18N
  return $ do
    courseForm $ table tableId (className groupSubmissionTable) # informationalTable $ do
      checkedUserScript
      headLine (stiCourse s)
      assignmentLine msg
      mapM_ (userLine s) (stiUserLines s)
  where
    -- JavaScript
    tableIdJSName = filter isAlphaNum tableId
    noOfUsers = tableIdJSName ++ "NoOfUsers"
    onCheck = tableIdJSName ++ "OnCheck"
    onUncheck = tableIdJSName ++ "OnUncheck"
    removeButton = tableIdJSName ++ "Button"
    onClick = tableIdJSName ++ "OnClick"
    checkedUserScript = H.script $ fromString $ unlines
      [ concat ["var ", noOfUsers, " = 0;"]
      , concat ["function ", onCheck, "(){"]
      ,   noOfUsers ++ "++;"
      ,   concat ["if(", noOfUsers, " > 0) {"]
      ,     concat ["document.getElementById(\"",removeButton,"\").disabled = false;"]
      ,   "}"
      , "}"
      , concat ["function ", onUncheck, "(){"]
      ,   noOfUsers ++ "--;"
      ,   concat ["if(", noOfUsers, " < 1) {"]
      ,     concat ["document.getElementById(\"",removeButton,"\").disabled = true;"]
      ,     noOfUsers ++ " = 0;"
      ,   "}"
      , "}"
      , concat ["function ", onClick, "(checkbox){"]
      ,   "if(checkbox.checked) {"
      ,      onCheck ++ "();"
      ,   "} else {"
      ,      onUncheck ++ "();"
      ,   "}"
      , "}"
      ]

    -- HTML
    courseForm = submissionTableInfoCata course group s where
      course _n _c _us _as _uls _ns ck      = postForm (routeOf $ Pages.deleteUsersFromCourse ck ())
      group _n _c _us _cgas _uls _ns _ck gk = postForm (routeOf $ Pages.deleteUsersFromGroup gk ())

    headerCell = H.th # (informationalCell <> grayBackground)

    assignmentLine msg = H.tr $ do
      headerCell $ fromString $ msg $ Msg_Home_SubmissionTable_StudentName "Name"
      headerCell $ fromString $ msg $ Msg_Home_SubmissionTable_Username "Username"
      assignmentLinks
      deleteHeaderCell msg
      where
        openedHeaderCell o c (_i,ak) =
          if isActiveAssignment ak
            then (H.th # (informationalCell <> o))
            else (H.th # (informationalCell <> c))

        assignmentLinks = submissionTableInfoCata course group s

        course _name _cfg _users as _ulines _anames _key =
          mapM_ (\x -> openedHeaderCell openCourseAssignmentStyle closedCourseAssignmentStyle x
                         $ modifyAssignmentLink "" x)
              $ zip [1..] as

        group  _name _cfg _users cgas _ulines _anames ckey _gkey = do
          let as = reverse . snd $ foldl numbering ((1,1),[]) cgas
          mapM_ header as
          where
            numbering ((c,g),as) = cgInfoCata
              (\ak -> ((c+1,g),(CourseInfo (c,ak):as)))
              (\ak -> ((c,g+1),(GroupInfo  (g,ak):as)))

            header = cgInfoCata
              (\x -> openedHeaderCell openCourseAssignmentStyle closedCourseAssignmentStyle x $
                       viewAssignmentLink ckey (msg $ Msg_Home_CourseAssignmentIDPreffix "C") x)
              (\x -> openedHeaderCell openGroupAssignmentStyle closedGroupAssignmentStyle x $
                       modifyAssignmentLink (msg $ Msg_Home_GroupAssignmentIDPreffix "G") x)

    assignmentName ak = maybe "" E.assignmentName . Map.lookup ak $ stiAssignmentInfos s

    isActiveAssignment ak = maybe False isActive . Map.lookup ak $ stiAssignmentInfos s
      where
        isActive a = and [E.assignmentStart a < now, now < E.assignmentEnd a]

    modifyAssignmentLink pfx (i,ak) =
      linkWithTitle
        (routeOf $ Pages.modifyAssignment ak ())
        (assignmentName ak)
        (concat [pfx, show i])


    viewAssignmentLink ck pfx (i,ak) =
      linkWithTitle
        (viewOrModifyAssignmentLink ck ak)
        (assignmentName ak)
        (concat [pfx, show i])
      where
        viewOrModifyAssignmentLink ck ak =
          case Map.lookup ck (stcAdminCourses ctx) of
            Nothing -> routeOf $ Pages.viewAssignment ak ()
            Just _  -> routeOf $ Pages.modifyAssignment ak ()

    userLine s (u,_p,submissionInfoMap) = do
      H.tr $ do
        let username = ud_username u
        dataCell noStyle . fromString $ ud_fullname u
        dataCell noStyle . fromString $ usernameCata id username
        submissionCells username s
        deleteUserCheckbox u
      where
        submissionInfos = submissionTableInfoCata course group where
          course _n _c _users as _ulines _anames _key =
            catMaybes $ map (\ak -> Map.lookup ak submissionInfoMap) as

          group _n _c _users as _ulines _anames _ckey _gkey =
            catMaybes $ map lookup as
            where
              lookup = cgInfoCata (const Nothing) (flip Map.lookup submissionInfoMap)


        submissionCells username = submissionTableInfoCata course group where
          course _n _c _users as _ulines _anames _key = mapM_ (submissionInfoCell username) as

          group _n _c _users as _ulines _anames _ck _gk =
            mapM_ (cgInfoCata (submissionInfoCell username) (submissionInfoCell username)) as

        submissionInfoCell u ak = case Map.lookup ak submissionInfoMap of
          Nothing -> dataCell noStyle $ fromString "░░░"
          Just si -> submissionCell u (ak,si)

    submissionCell u (ak,si) =
      coloredSubmissionCell
        dataCell
        (H.td)
        (linkWithText (routeWithParams (Pages.userSubmissions ()) [requestParam u, requestParam ak]))
        "░░░" -- not found
        "░░░" -- non-evaluated
        "░░░" -- tested
        "░░░" -- accepted
        "░░░" -- rejected
        si    -- of percent

    deleteHeaderCell msg = submissionTableInfoCata deleteForCourseButton deleteForGroupButton s where
        deleteForCourseButton _n _c _us _as _uls _ans _ck =
          headerCell $ submitButton
            removeButton
            (msg $ Msg_Home_DeleteUsersFromCourse "Remove") ! A.disabled ""

        deleteForGroupButton _n _c _us _as _uls _ans _ck _gk =
          headerCell $ submitButton
            removeButton
            (msg $ Msg_Home_DeleteUsersFromGroup "Remove") ! A.disabled ""

    deleteUserCheckbox u = submissionTableInfoCata deleteCourseCheckbox deleteGroupCheckbox s where
        deleteCourseCheckbox _n _c _us _as _uls _ans _ck =
          dataCell noStyle $ checkBox
            (Param.name delUserFromCoursePrm)
            (encode delUserFromCoursePrm $ ud_username u)
            False ! A.onclick (fromString (onClick ++ "(this)"))

        deleteGroupCheckbox _n _c _us _as _uls _ans _ck _gk =
          dataCell noStyle $ checkBox
            (Param.name delUserFromGroupPrm)
            (encode delUserFromGroupPrm $ ud_username u)
            False ! A.onclick (fromString (onClick ++ "(this)"))

-- Create a table cell for the evaulation value, where
-- simpleCell is the combinator for the non RGB colored cells
-- rgbCell is a cell combinator where the rgb value will be set
-- content how the computed text value is wrapped
-- notFound text for the non evaulated submission
-- unevaluated text for the unevaluated submission
-- passed message for the passed binary evaulation
-- failed message for the failed binary evaulation
-- s the submission information itself
coloredSubmissionCell simpleCell rgbCell content notFound unevaluated tested passed failed s =
  coloredCell $ content (sc s)
  where
    sc = submissionInfoCata
           notFound
           unevaluated
           tested
           (\_key result -> val result) -- evaluated

    val (BinEval (Binary Passed)) = passed
    val (BinEval (Binary Failed)) = failed
    val (PctEval (Percentage (Scores [p]))) = percent p
    val (PctEval (Percentage _)) = error "SubmissionTable.coloredSubmissionCell percentage is not defined"

    coloredCell = color s

    color =
      submissionInfoCata
        (simpleCell noStyle)        -- Not Found
        (simpleCell unevaluatedStyle) -- Unevulated
        (simpleCell testedStyle)      -- Tested
        (const resultCell)        -- Result

    resultCell (BinEval (Binary Passed)) = simpleCell binaryPassedStyle
    resultCell (BinEval (Binary Failed)) = simpleCell binaryFailedStyle
    resultCell p@(PctEval {}) = withRGBClass (EvResult p) rgbCell

    percent x = join [show . round $ (100 * x), "%"]

    withRGBClass r = maybe id (\pct html -> html ! (A.style . fromString . colorStyle . pctCellColor $ pct)) (percentValue r)

courseTestScriptTable :: CourseTestScriptInfos -> SubmissionTableInfo -> IHtml
courseTestScriptTable cti = submissionTableInfoCata course group where
  course _n _c _us _as _uls _ans ck = testScriptTable cti ck
  group _n _c _us _as _uls _ans _ck _gk = (return (return ()))

-- Renders a course test script modification table if the information is found in the
-- for the course, otherwise an error message. If the course is found, and there is no
-- test script found for the course a message indicating that will be rendered, otherwise
-- the modification table is rendered
testScriptTable :: CourseTestScriptInfos -> CourseKey -> IHtml
testScriptTable cti ck = maybe (return "") courseFound $ Map.lookup ck cti where
  courseFound ts = do
    msg <- getI18N
    return $ do
      table tableId (className groupSubmissionTable) # informationalTable $ do
        headLine . msg $ Msg_Home_ModifyTestScriptTable "Testers"
        case ts of
          []  -> dataCell noStyle $ fromString . msg $
                   Msg_Home_NoTestScriptsWereDefined "There are no testers for the course."
          ts' -> mapM_ testScriptLine ts'
    where
      headLine = H.tr . (H.th # textAlign "left" ! A.colspan "4") . fromString
      tableId = join ["tst-", courseKeyMap id ck]
      dataCell r = H.td # (informationalCell <> r)

      testScriptLine (tsk,tsi) = do
        dataCell noStyle $ linkWithText
          (routeOf (Pages.modifyTestScript tsk ()))
          (tsiName tsi)

-- Renders a menu for the creation of the course or group assignment if the
-- user administrates the given group or course
assignmentCreationMenu
  :: AdministratedCourses
  -> AdministratedGroups
  -> SubmissionTableInfo
  -> IHtml
assignmentCreationMenu courses groups = submissionTableInfoCata courseMenu groupMenu
  where
    groupMenu _n _c _us _as _uls _ans ck gk = maybe
      (return (return ()))
      (const $ do
        msg <- getI18N
        return . navigationWithRoute msg $
          case Map.lookup ck courses of
            Nothing -> [Pages.newGroupAssignment gk ()]
            Just _  -> [Pages.newGroupAssignment gk (), Pages.newCourseAssignment ck ()] )
      (Map.lookup gk groups)

    courseMenu _n _c _us _as _uls _ans ck = maybe
      (return (return ()))
      (const $ do
        msg <- getI18N
        return (navigationWithRoute msg [Pages.newCourseAssignment ck ()]))
      (Map.lookup ck courses)

    navigationWithRoute msg links = H.div ! A.id "menu" $ H.ul $ mapM_ elem links
      where
        elem page = link (routeOf page) (msg $ linkText page)



-- * CSS Section

openCourseAssignmentStyle = backgroundColor "#52B017"
openGroupAssignmentStyle  = backgroundColor "#00FF00"
closedCourseAssignmentStyle = backgroundColor "#736F6E"
closedGroupAssignmentStyle = backgroundColor "#A3AFAE"

binaryPassedStyle = backgroundColor "lightgreen"
binaryFailedStyle = backgroundColor "red"
unevaluatedStyle  = backgroundColor "gray"
testedStyle       = backgroundColor "yellow"

summaryPassedStyle = backgroundColor "lightgreen"
summaryFailedStyle = backgroundColor "red"
summaryErrorStyle  = backgroundColor "yellow"

-- * Colors

newtype RGB = RGB (Int, Int, Int)

pctCellColor :: Double -> RGB
pctCellColor x = RGB (round ((1 - x) * 255), round (x * 255), 0)

colorStyle :: RGB -> String
colorStyle (RGB (r,g,b)) = join ["background-color:#", hex r, hex g, hex b]
  where
    twoDigits [d] = ['0',d]
    twoDigits ds  = ds

    hex x = twoDigits (showHex x "")

-- * Tools

sortUserLines = submissionTableInfoCata course group where
  course name cfg users assignments userlines names key =
      CourseSubmissionTableInfo name cfg users assignments (sort userlines) names key

  group name cfg users assignments userlines names ckey gkey =
      GroupSubmissionTableInfo name cfg users assignments (sort userlines) names ckey gkey

  sort = sortBy (compareHun `on` fst3)

  fst3 :: (a,b,c) -> a
  fst3 (x,_,_) = x

submissionTableInfoAssignments = submissionTableInfoCata course group where
  course _n _c _us as _uls _ans _ck = as
  group _n _c _us cgas _uls _ans _ck _gk = map (cgInfoCata id id) cgas

headLine   = H.tr . (H.th # textAlign "left" ! A.colspan "4") . fromString
dataCell r = H.td # (informationalCell <> r)
