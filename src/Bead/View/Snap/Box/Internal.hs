module Bead.View.Box.Internal

-- * Declarations

class Box b where
  top    :: b a -> a
  right  :: b a -> a
  botton :: b a -> a
  left   :: b a -> a

type Qrt a = (a,a,a,a)
  
newtype Padding a = Padding (Qrt a)
newtype Border  a = Border  (Qrt a)
newtype Margin  a = Margin  (Qrt a)

data ContentBox p b m 
  = ContentBox {
      padding :: Padding p
    , border  :: Border  b
    , margin  :: Margin  m
    } 
  | Horizonal (ContentBox p b m) (ContentBox p b m)
  | Vertical  (ContentBox p b m) (ContentBox p b m)
  | Empty

-- * Box instances

instance Box Padding where
  top    (Padding (t,r,b,l)) = t
  right  (Padding (t,r,b,l)) = r
  botton (Padding (t,r,b,l)) = b
  left   (Padding (t,r,b,l)) = l

instance Box Border where
  top    (Border (t,r,b,l)) = t
  right  (Border (t,r,b,l)) = r
  botton (Border (t,r,b,l)) = b
  left   (Border (t,r,b,l)) = l

instance Box Margin where
  top    (Margin (t,r,b,l)) = t
  right  (Margin (t,r,b,l)) = r
  botton (Margin (t,r,b,l)) = b
  left   (Margin (t,r,b,l)) = l

