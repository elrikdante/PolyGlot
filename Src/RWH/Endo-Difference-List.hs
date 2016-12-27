{-# LANGUAGE RankNTypes #-}
module Data.DList.Endo where

import Data.Functor.Constant
import Data.Monoid
import Data.Function

newtype EDList a = EDL { unEDL :: forall b . Constant (Endo [a]) b }

instance Monoid (EDList a) where
  mempty = empty
  mappend = append

singleton :: a -> EDList a
singleton x = EDL (Constant (Endo (x:)))

empty :: EDList a
empty  = EDL (Constant (Endo id))

cons :: a -> EDList a -> EDList a
cons x (EDL xs) = EDL (Constant (Endo (x:) <> getConstant xs))

toList :: EDList a -> [a]
toList = flip appEndo [] . getConstant . unEDL

fromList,fromList' :: [a] -> EDList a
fromList' = Prelude.foldr cons empty
fromList  as = EDL (Constant (Endo (as++)))

append :: EDList a -> EDList a -> EDList a
append (EDL xs) (EDL ys) = EDL (xs <> ys)

foldr     :: (a -> b -> b) -> b -> EDList a -> b
foldr f z = Prelude.foldr f z . toList

map  :: (a -> b) -> EDList a -> EDList b
map f = Data.DList.Endo.foldr (cons . f) empty

repeat :: a -> EDList a
repeat x = EDL (Constant (Endo (const (fix (x:)))))

replicate :: Int -> a -> EDList a
replicate 0 _ = EDL (Constant (Endo id))
replicate n a = cons a (Data.DList.Endo.replicate (n - 1) a)
            
take :: Int -> EDList a -> EDList a
take n  = fromList . Prelude.take n . toList
