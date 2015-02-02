module QuickFind
       (QuickFind (..),
        Node (..),
        mkQF,
        union,
        isConnected)
       where

import qualified Data.Sequence as S

newtype QuickFind a = QuickFind (S.Seq (Node a))
                      deriving (Show, Eq)

data Node a =   Info { ptr :: Int, descr :: a }
              | Link { ptr :: Int, descr :: a }
              deriving (Show,Eq)

-- O(n). initialise a QuickFind structure holding descriptors of type a
-- The nodes are initialised as Infos meaning they are Trees, thier ptr (Pointer) points to
-- themselves.
mkQF :: Int -> a -> QuickFind a
mkQF _N d = QuickFind $ S.fromList [Info {descr = d, ptr = n} | n <- [0..(_N-1)]]

-- O(1). find wheter two nodes are in the same equivalence set based on their index in the sequence
isConnected :: (Eq a) => Int -> Int -> QuickFind a -> Bool
isConnected n m qf = let
  n' = find n qf
  m' = find m qf
  in ptr n' == ptr m'

-- O(n). point all pointers of n to point at m's ptr.
-- Worst case analysis is n - 1
union :: (Eq a) => Int -> Int -> QuickFind a -> QuickFind a
union n m qf@(QuickFind seq) =  QuickFind $ S.mapWithIndex (\_ node -> case all filterFn [node] of
                                       False  -> node
                                       True -> Link {ptr = ptrm, descr = descr node}
                                 ) seq
  where
    ptrn        = ptr $ find n qf
    ptrm        = ptr $ find m qf
    filterFn  x = (ptr x == ptrn) && (not $ isConnected ptrn ptrm qf)

-- O(1). retrieve the element indexed at n
find :: (Eq a) => Int -> QuickFind a -> Node a
find n (QuickFind seq) =  S.index seq n

-- O(n). Retrive connected components
