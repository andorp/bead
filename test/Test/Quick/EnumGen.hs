module Test.Quick.EnumGen where

import Test.QuickCheck.Gen

enumGenerator :: (Enum e) => Gen e
enumGenerator = elements [(toEnum 0) .. ]
