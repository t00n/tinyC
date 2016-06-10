module Graph where

import qualified Data.Set as S
import qualified Data.Map as M

data Graph a = Graph (S.Set Node) (S.Set Edge) (M.Map Node a)
    deriving (Eq, Show)

type Node = Int

type Edge = (Int, Int)

lookupNode :: Ord a => Node -> Graph a -> Maybe a
lookupNode i (Graph _ _ values) = M.lookup i values

unsafeLookupNode :: Ord a => Node -> Graph a -> a
unsafeLookupNode n g = 
    case lookupNode n g of
        Nothing -> error "in unsafeLookupNode"
        (Just x) -> x

insertNode :: Ord a => Node -> a -> Graph a -> Graph a
insertNode x v (Graph nodes edges values) = Graph (S.insert x nodes) edges (M.insert x v values)

insertEdge :: Ord a => Int -> Int -> Graph a -> Graph a
insertEdge p c (Graph nodes edges values) = Graph nodes (S.insert (p, c) edges) values

delete :: Ord a => Int -> Graph a -> Graph a
delete n (Graph nodes edges values) = Graph newnodes newedges newvalues
    where
        newnodes = S.delete n nodes
        newedges = S.filter (\(a, b) -> a /= n && b /= n) edges
        newvalues = M.delete n values 

emptyGraph :: Graph a
emptyGraph = Graph S.empty S.empty M.empty

predecessors :: Ord a => Node -> Graph a -> S.Set Node
predecessors n (Graph _ edges _) = S.map fst $ S.filter (((==) n) . snd) edges

successors :: Ord a => Node -> Graph a -> S.Set Node
successors n (Graph _ edges _) = S.map snd $ S.filter (((==) n) . fst) edges

neighbours :: Ord a => Node -> Graph a -> S.Set Node
neighbours n g = predecessors n g `S.union` successors n g

subgraph :: Ord a => [a] -> Graph a -> Graph a
subgraph xs (Graph nodes edges values) = 
    let subGraphValues = M.filter (`elem` xs) values
        subGraphNodes = S.filter (`M.member` subGraphValues) nodes
        subGraphEdges = S.filter (\(p, c) -> S.member p subGraphNodes && S.member c subGraphNodes) edges
    in Graph subGraphNodes subGraphEdges subGraphValues