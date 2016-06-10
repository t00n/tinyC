module Graph where

import qualified Data.Set as S
import qualified Data.Map as M

data Graph a = Graph (Nodes a) (Edges a)
    deriving (Eq, Show)
type Nodes a = S.Set a
type Edge a = (a, a)
type Edges a = S.Set (Edge a)

insertNode :: Ord a => a -> Graph a -> Graph a
insertNode x (Graph nodes edges) = Graph (S.insert x nodes) edges

insertEdge :: Ord a => a -> a -> Graph a -> Graph a
insertEdge p c (Graph nodes edges) = Graph nodes (S.insert (p, c) edges)

deleteNode :: Ord a => a -> Graph a -> Graph a
deleteNode n (Graph nodes edges) = Graph newnodes newedges
    where
        newnodes = S.delete n nodes
        newedges = S.filter (\(a, b) -> a /= n && b /= n) edges

emptyGraph :: Graph a
emptyGraph = Graph S.empty S.empty

predecessors :: Ord a => a -> Graph a -> S.Set a
predecessors n (Graph _ edges) = S.map fst $ S.filter (((==) n) . snd) edges

successors :: Ord a => a -> Graph a -> S.Set a
successors n (Graph _ edges) = S.map snd $ S.filter (((==) n) . fst) edges

neighbours :: Ord a => a -> Graph a -> S.Set a
neighbours n g = predecessors n g `S.union` successors n g

subgraph :: Ord a => [a] -> Graph a -> Graph a
subgraph xs (Graph nodes edges) = 
    let subGraphNodes = S.filter (`elem` xs) nodes
        subGraphEdges = S.filter (\(p, c) -> S.member p subGraphNodes && S.member c subGraphNodes) edges
    in Graph subGraphNodes subGraphEdges