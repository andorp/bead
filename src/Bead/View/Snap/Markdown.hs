module Bead.View.Snap.Markdown where

{- A markdown to HTML conversion. -}

import Text.Pandoc.Options (def, writerHtml5)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml)

import Text.Blaze.Html5 (Html)

-- Produces an HTML value from the given markdown formatted strings
markdownToHtml :: String -> Html
markdownToHtml = writeHtml def . readMarkdown def

