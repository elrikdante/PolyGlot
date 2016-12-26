import Data.Monoid
import Data.Function

newtype EDList a = EDL { unEDL :: Endo [a] }

instance Monoid (EDList a) where
  mempty = empty
  mappend = append

singleton :: a -> EDList a
singleton = EDL . Endo . (:)

empty :: EDList a
empty  = EDL (Endo id)

cons :: a -> EDList a -> EDList a
cons x (EDL xs) = EDL (Endo (x:) <> xs)

toList :: EDList a -> [a]
toList = flip appEndo [] . unEDL

fromList,fromList' :: [a] -> EDList a
fromList' = Prelude.foldr cons empty
fromList  = EDL . Endo . (++)

append :: EDList a -> EDList a -> EDList a
append (EDL xs) (EDL ys) = EDL (xs <> ys)

foldr     :: (a -> b -> b) -> b -> EDList a -> b
foldr f z = Prelude.foldr f z . toList

map  :: (a -> b) -> EDList a -> EDList b
map f = Main.foldr (cons . f) empty

repeat :: a -> EDList a
repeat = EDL . Endo . const . fix . (:)

replicate 0 _ = EDL (Endo id)
replicate n a = singleton a <> Main.replicate (n - 1) a
            
take :: Int -> EDList a -> EDList a
take n = fromList . Prelude.take n . toList

listA = fromList [1..5]
listB = fromList [10..20]
