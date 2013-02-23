module Test.Quick.EnumGen where

import Test.QuickCheck.Gen

enumElements :: (Enum e) => Gen e
enumElements = elements [(toEnum 0) .. ]
