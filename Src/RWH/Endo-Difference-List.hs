import Data.Monoid

newtype EDList a = EDL { unEDL :: Endo [a] }

singleton :: a -> EDList a
singleton = EDL . Endo . (:)

empty :: EDList a
empty  = EDL (Endo id)

cons :: a -> EDList a -> EDList a
cons x (EDL xs) = EDL (Endo (x:) <> xs)

toList :: EDList a -> [a]
toList = flip appEndo [] . unEDL

fromList :: [a] -> EDList a
fromList = Prelude.foldr go empty where go = cons

append :: EDList a -> EDList a -> EDList a
append (EDL xs) (EDL ys) = EDL (xs <> ys)

foldr :: (a -> b -> b) -> b -> EDList a -> b
foldr f z = Prelude.foldr f z . toList

map  :: (a -> b) -> EDList a -> EDList b
map f = Main.foldr go empty where go = cons . f

replicate :: a -> EDList a
replicate = EDL . Endo . const . fix . (:)

take :: Int -> EDList a -> EDList a
take n = fromList . Prelude.take n . toList

listA = fromList [1..5]
listB = fromList [10..20]
