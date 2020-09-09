module Lens_State where

import Control.Lens

cross :: (a1 -> b1) -> (a2 -> b2) -> (a1,a2) -> (b1,b2)
cross f g = over _1 f . over _2 g

newtype State s a = St{unSt :: s -> (s,a)}

newtype Counter a = C{unC :: State Int a}

instance Functor Counter where
    fmap f (C  (St p)) = C . St $ (cross id f) . p

-- 対角射
diagonal :: a -> (a,a)
diagonal x = (x,x)
-- 積の普遍射
produniv :: (a1 -> b1) -> (a1 -> b2) -> a1 -> (b1,b2)
produniv f g = cross f g . diagonal
-- 評価射
eval :: (a,a->b) -> b
eval = uncurry (flip ($))

instance Applicative Counter where
    pure x = C $ St (\s -> (s,x))
    (C (St f)) <*> (C (St x)) = let p1 = view _1 . f
                                    p2 = view _2 . f
                                    t1 = view _1 . x
                                    t2 = view _2 . x
                                in C $ St (produniv (t1 . p1) (eval . produnive (t2 . p1) p2))

