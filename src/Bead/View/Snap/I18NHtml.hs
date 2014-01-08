module Bead.View.Snap.I18NHtml where

import qualified Text.Blaze.Html5 as H
import Bead.View.Snap.I18N

{-
Mimicking the HTML Blaze combinators within the I18NHtml monad.
-}

-- * HTML tags

a      = lh2 H.a
{-# INLINE a #-}

b      = lh2 H.b
{-# INLINE b #-}

body   = lh2 H.body
{-# INLINE body #-}

br     = lh  H.br
{-# INLINE br #-}

div    = lh2 H.div
{-# INLINE div #-}

docTypeHtml = lh2 H.docTypeHtml
{-# INLINE docTypeHtml #-}

form   = lh2 H.form
{-# INLINE form #-}

head   = lh2 H.head
{-# INLINE head #-}

hr     = lh  H.hr
{-# INLINE hr #-}

h2     = lh2 H.h2
{-# INLINE h2 #-}

h3     = lh2 H.h3
{-# INLINE h3 #-}

input  = lh  H.input
{-# INLINE input #-}

li     = lh2 H.li
{-# INLINE li #-}

link   = lh  H.link
{-# INLINE link #-}

meta   = lh  H.meta
{-# INLINE meta #-}

option = lh2 H.option
{-# INLINE option #-}

p      = lh2 H.p
{-# INLINE p #-}

pre    = lh2 H.pre
{-# INLINE pre #-}

script = lh2 H.script
{-# INLINE script #-}

select = lh2 H.select
{-# INLINE select #-}

small  = lh2 H.small
{-# INLINE small #-}

span   = lh2 H.span
{-# INLINE span #-}

table  = lh2 H.table
{-# INLINE table #-}

textarea = lh2 H.textarea
{-# INLINE textarea #-}

title  = lh2 H.title
{-# INLINE title #-}

td     = lh2 H.td
{-# INLINE td #-}

th     = lh2 H.th
{-# INLINE th #-}

tr     = lh2 H.tr
{-# INLINE tr #-}

ul     = lh2 H.ul
{-# INLINE ul #-}

