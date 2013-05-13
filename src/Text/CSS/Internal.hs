{-# LANGUAGE OverloadedStrings, ExistentialQuantification, 
             DeriveDataTypeable, FlexibleInstances #-}
module Text.CSS.Internal where

import Data.String
import Data.Typeable
import Control.Monad (join)

-- * Declarations

newtype Selector = Selector String

newtype PseudoClass = PseudoClass String

newtype Property = Property (String, String)

data Element = Element {
    selectors   :: [Selector]
  , properties  :: [Property]
  }

data CSS = CSS {
    elements :: [Element]
  }

isEmptyCSS :: CSS -> Bool
isEmptyCSS = null . elements

-- * Selector functions

desc :: Selector -> Selector -> Selector
desc (Selector p) (Selector c) = Selector (join [p, " ", c])

child :: Selector -> Selector -> Selector
child (Selector p) (Selector c) = Selector (join [p, " > ", c])

pseudoClass :: Selector -> String -> Selector
pseudoClass (Selector s) c = Selector (join [s, ":", c])

-- * IsString instances

instance IsString Selector where
  fromString = Selector

instance IsString PseudoClass where
  fromString = PseudoClass

-- * Css Monad

data CssM a
  = CssEmpty
  | forall b c . CssAppend (CssM b) (CssM c)
  | CssElement Element
  deriving (Typeable)

type Css = CssM ()

instance Monad CssM where
  return _ = CssEmpty
  (>>) = CssAppend
  h1 >>= f = h1 >> f (error "CssM: invalid use of monadic bind")

runCssM :: CssM a -> CSS
runCssM = CSS . runElements
  where
    runElements :: CssM a -> [Element]
    runElements CssEmpty = []
    runElements (CssAppend a b) = runElements a ++ runElements b
    runElements (CssElement e) = [e]

-- * Poperty Monad

data PropertyM a
  = PEmpty
  | forall b c . PAppend (PropertyM b) (PropertyM c)
  | PElement Property
  deriving (Typeable)

instance Monad PropertyM where
  return _ = PEmpty
  (>>) = PAppend
  h1 >>= f = h1 >> f (error "PropertyM: invalid use of monadic bind")

infixl 2 <:>

(<:>) :: String -> String -> PropertyM ()
(<:>) = property

property :: String -> String -> PropertyM ()
property p v = PElement $ Property (p,v)

runPropertyM :: PropertyM a -> [Property]
runPropertyM PEmpty = []
runPropertyM (PAppend a b) = runPropertyM a ++ runPropertyM b
runPropertyM (PElement p) = [p]

-- * Selector monad

data SelectorM a
  = SEmpty
  | forall b c . SAppend (SelectorM b) (SelectorM c)
  | SElement Selector
  deriving (Typeable)

instance Monad SelectorM where
  return _ = SEmpty
  (>>) = SAppend
  h1 >>= f = h1 >> f (error "SelectorM: invalid use of monadic bind")

instance IsString (SelectorM a) where
  fromString = SElement . fromString

class MonadSelector s where
  selector :: s -> SelectorM ()

instance MonadSelector Selector where
  selector = SElement

runSelectorM :: SelectorM a -> [Selector]
runSelectorM SEmpty = []
runSelectorM (SAppend a b) = runSelectorM a ++ runSelectorM b
runSelectorM (SElement s) = [s]

infixl 2 <&>

(<&>) :: SelectorM a -> PropertyM b -> CssM ()
s <&> p = CssElement $ Element {
    selectors   = runSelectorM s
  , properties  = runPropertyM p
  }
