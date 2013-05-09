module Text.CSS.Pretty where

import Data.List (intersperse)
import Control.Monad (join)
import Text.CSS.Internal


class Pretty p where
  pretty :: p -> String

instance Pretty Selector where
  pretty (Selector s) = s

instance Pretty PseudoClass where
  pretty (PseudoClass c) = c

instance Pretty Property where
  pretty (Property (p,v)) = join [p, ": ", v, ";"]

instance Pretty Element where
  pretty e = join
      [ (join . intersperse ", " . map pretty . selectors $ e)
      , " "
      , (maybe "" (\c -> join [":", pretty c]) . pseudoClass $ e)
      , " {\n"
      , join (map (\p -> join ["  ", pretty p, "\n"]) . properties $ e)
      , "}\n"
      ]

instance Pretty CSS where
  pretty = join . map pretty . elements

renderCSS :: Css -> String
renderCSS = pretty . runCssM
