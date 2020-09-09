module Lens_State where

import Control.Lens

cross :: (a1 -> b1) -> (a2 -> b2) -> (a1,a2) -> (b1,b2)
cross f g = over _1 f . over _2 g

newtype State s a = St{unSt :: s -> (s,a)}

newtype Counter a = C{unC :: State Int a}

instance Functor Counter where
    fmap f (C  (St p)) = C . St $ (cross id f) . p

-- 対角射
diagonal x = (x,x)
-- 積の普遍射
produniv f g = cross f g . diagonal