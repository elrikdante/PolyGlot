module QuickUnion
       (mkQU, union, isConnected, root)
       where
import qualified Data.Sequence as S

newtype QuickUnion a = QuickUnion (S.Seq (Node a)) deriving (Eq, Show)

data Node a =   Info {descr :: a }
              | Link {rootTree :: Int, descr :: a }
                deriving (Eq, Show)

-- O(n). initialise a QuickUnion Int with _N objects
mkQU :: (Eq a, Num a) => Int -> a -> QuickUnion Int
mkQU _N base = QuickUnion $ S.fromList [Info {descr = n} | n <- [0..(_N-1)]]

-- O(n). find the root of a node
-- asymptotic behaviour tends towards N in the case of a tall tree
root :: (Eq a, Num a) => Int -> QuickUnion a -> Node a
root n qf = case node of
  (Link {rootTree = r})  -> root r qf
  (Info {})              -> node
  where             
    node = find n qf



-- O(n). assign the root of one element to another.
-- in this case modify n's root to point to m's root.
union :: (Eq a, Num a) => Int -> Int -> QuickUnion Int -> QuickUnion Int
union n m qf@(QuickUnion seq) =
  let
    n' = root n qf
    m' = root m qf 
  in case isConnected n m qf of
    True -> qf
    False -> QuickUnion $ S.update n (Link {rootTree = descr m'  , descr = descr n'} ) seq

-- O(n). determine whether two nodes are connected
-- time complexity proportional to depth of n and m
isConnected :: (Eq a, Num a) => Int -> Int -> QuickUnion a -> Bool
isConnected n m qf = (root n qf) == (root m qf)

-- O(1). find an N
find :: Int -> QuickUnion a -> Node a
find n qf@(QuickUnion seq) = S.index seq n
