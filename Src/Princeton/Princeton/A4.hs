module Princeton.A4
(QuickFind (..),
QuickUnion (..),
Node (..),
initQF)

where
import qualified Data.Sequence as S


data QuickFind a = QuickFind (S.Seq (Node a))  deriving (Show)
data Node a      = Node a a | Root a deriving (Show)

type NodeLength = Int

initQF n = QuickFind nodes
  where
    nodes = S.iterateN n ((.) Root succ) (Root (-1))
    
--union :: QuickFind a -> Node a -> Node a -> QuickFind a
--union (QuickFind l nodes) (Node a) (Node b) = QuickFind l nodes
