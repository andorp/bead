module Bead.View.Snap.Markdown (
    markdownToHtml
  ) where

{- A markdown to HTML conversion. -}

import Data.String.Utils (replace)
import Text.Pandoc.Options (def, writerHtml5)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml)

import Text.Blaze.Html5 (Html)

-- Produces an HTML value from the given markdown formatted strings what
-- comes from a text area field, crlf endings must be replaced with lf in the string
markdownToHtml :: String -> Html
markdownToHtml = writeHtml def . readMarkdown def . replaceCrlf

replaceCrlf :: String -> String
replaceCrlf = replace "\r\n" "\n"

