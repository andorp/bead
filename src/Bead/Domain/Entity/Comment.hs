{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Entity.Comment where

import           Control.Applicative
import           Data.Data
import           Data.Time (UTCTime)

#ifdef TEST
import           Test.Tasty.Arbitrary
#endif


-- Comment type basically indicates that who left the comment,
-- constructors are self explanatories
data CommentType
  = CT_Student
  | CT_GroupAdmin
  | CT_CourseAdmin
  | CT_Admin
  deriving (Data, Eq, Read, Show, Typeable)

commentTypeCata
  student
  groupAdmin
  courseAdmin
  admin
  c = case c of
    CT_Student     -> student
    CT_GroupAdmin  -> groupAdmin
    CT_CourseAdmin -> courseAdmin
    CT_Admin       -> admin

#ifdef TEST
instance Arbitrary CommentType where
  arbitrary = elements [CT_Student, CT_GroupAdmin, CT_CourseAdmin, CT_Admin]
  shrink = commentTypeCata
    [CT_GroupAdmin, CT_CourseAdmin, CT_Admin]
    [CT_CourseAdmin, CT_Admin]
    [CT_Admin]
    []
#endif

-- | Comment on the text of exercise, on the evaluation
data Comment = Comment {
    comment       :: String
  , commentAuthor :: String
  , commentDate   :: UTCTime
  , commentType   :: CommentType
  } deriving (Eq, Show)

commentCata f (Comment c a d t) = f c a d t

commentAna comment author date type_ =
  Comment <$> comment <*> author <*> date <*> type_

-- Returns True if the comment can be displayed for the student
-- otherwise false
isStudentComment :: Comment -> Bool
isStudentComment = commentCata $ \_comment _owner _date -> student where
  student = commentTypeCata
    True  -- Student
    True  -- Group Admin
    True  -- Course Admin
    False -- Admin
