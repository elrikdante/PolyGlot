module Princeton.A4

(QuickFind (..),
QuickUnion (..),
Node (..),
initQF)

where

import qualified Data.Sequence as S

data QuickFind a  = QuickFind (S.Seq (Node a))  deriving (Show)
data QuickUnion a = QuickUnion a deriving (Show)
data Node a       = Node RootID a | Root RootID a deriving (Show)

type NodeLength = Int
type RootID     = Int
type NodeID     = Int

initQF       :: Int -> a -> QuickFind a
initQF n base = QuickFind nodes
  where
    rootID :: RootID
    rootID = 0
    nodes  = S.iterateN n (\(Root id v) -> Root (succ id) v) (Root rootID base)

runQF
  :: QuickFind a
     -> S.Seq (Node a)
runQF (QuickFind s) = s

connectedComponents
  :: QuickFind a
  -> S.Seq (Node a)

connectedComponents (QuickFind seq) = S.filter rootFilterFn seq
                                      where
                                        rootFilterFn node = case node of
                                                           Root _ _ -> True
                                                           _        -> False                                                        
root
  :: QuickFind a
     -> Node a
     -> RootID

root _ node@(Root id _)            = id
root qf@(QuickFind a) (Node r v)   = let node = S.index a r in root qf node

union
  :: QuickFind a
     -> NodeID
     -> NodeID
     -> QuickFind a

union qf@(QuickFind node_seq) n m = QuickFind node_seq'
                                     where
                                       n' = case (child) of
                                     
                                         Root r v  -> case (r == (root qf parent)) of
                                           True  -> Root r v
                                           False -> Node m v
                                       
                                         Node r a -> Node m a
                                       
                                       node_seq' = S.update n n' node_seq
                                       child     = S.index node_seq n
                                       parent    = S.index node_seq m
