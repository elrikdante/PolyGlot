{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Data.DList.Endo where

import Data.Functor.Constant
import Data.Monoid
import Data.Function

data None
data Some

data EDList a = forall b. EDL { unEDL :: ListOf b a }

type ListOf b a = Constant (Endo [a]) b

instance Monoid (EDList a) where
  mempty = empty
  mappend = append


singleton :: a -> EDList a
singleton x = EDL (Constant (Endo (x:)))

empty :: EDList a
empty  = EDL (Constant (Endo id))

cons :: a -> EDList a -> EDList a
cons x (EDL (Constant xs)) = EDL (Constant (Endo (x:) <> xs))

toList :: EDList a -> [a]
toList (EDL innards) = flip appEndo [] (getConstant innards)

fromList,fromList' :: [a] -> EDList a
fromList' = Prelude.foldr cons empty
fromList  as = EDL (Constant (Endo (as++)))

append :: EDList a -> EDList a -> EDList a
append (EDL (Constant xs)) (EDL (Constant ys)) = EDL (Constant (xs <> ys))

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

head :: EDList a -> a
head = Prelude.head . toList

--
class Coercible a b where
  coerce :: a -> b

class IsEmpty a where
  isEmpty :: a -> Bool

instance Coercible (ListOf None a) (ListOf Some a) where
  coerce = const (Constant (Endo id))

instance Coercible (ListOf Some a) (ListOf None a) where
  coerce = const nil

instance IsEmpty (ListOf None a) where
  isEmpty = const True

instance IsEmpty (ListOf Some a) where
  isEmpty = const False

instance IsEmpty None where
  isEmpty = const True

instance IsEmpty Some where
  isEmpty = const False

nil :: ListOf None a
nil = Constant (Endo id)

one :: a -> ListOf Some a
one x = Constant (Endo (x:))

inflate :: ListOf None a -> ListOf Some a
inflate= coerce

deflate :: ListOf Some a -> ListOf None a
deflate= coerce
