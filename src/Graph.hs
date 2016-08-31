module Graph where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Queue as Q

import Debug.Trace (traceShow)

data Graph a = Graph (Nodes a) Edges
    deriving (Eq, Show)
type Key = Int
type Nodes a = M.Map Key a
type Edge = (Key, Key)
type Edges = S.Set Edge

nodes :: Ord a => Graph a -> Nodes a
nodes (Graph nodes _) = nodes

edges :: Ord a => Graph a -> Edges
edges (Graph _ edges) = edges

generateKey :: Ord a => Graph a -> Key
generateKey g = M.size (nodes g)

keys :: Ord a => Graph a -> [Key]
keys = M.keys . nodes

keysSet :: Ord a => Graph a -> S.Set Key
keysSet (Graph nodes _) = M.keysSet nodes

values :: Ord a => Graph a -> [a]
values = M.elems . nodes

unsafeLookup :: Ord a => Key -> Graph a -> a
unsafeLookup i (Graph nodes _) = nodes M.! i

key :: Ord a => a -> Nodes a -> Key
key n = (head . (map fst) . (filter (((==) n) . snd)) . M.toList)

insertNode :: Ord a => a -> Graph a -> Graph a
insertNode x g@(Graph nodes edges) = 
    let key = generateKey g
    in Graph (M.insert key x nodes) edges

insertEdge :: Key -> Key -> Graph a -> Graph a
insertEdge p c (Graph nodes edges) = Graph nodes (S.insert (p, c) edges)

delete :: Key -> Graph a -> Graph a
delete k (Graph nodes edges) = 
    let newnodes = M.delete k nodes
        newedges = S.filter (\(a, b) -> a /= k && b /= k) edges
    in Graph newnodes newedges

deleteAll :: [Key] -> Graph a -> Graph a
deleteAll = flip (foldr delete)

deleteNode :: Ord a => a -> Graph a -> Graph a
deleteNode v g = delete (find v g) g

empty :: Graph a
empty = Graph M.empty S.empty

find :: Ord a => a -> Graph a -> Key
find n (Graph nodes _) = key n nodes

predecessors :: Key -> Graph a -> S.Set Key
predecessors k (Graph nodes edges) = S.map fst $ S.filter (((==) k) . snd) edges

successors :: Key -> Graph a -> S.Set Key
successors k (Graph nodes edges) = S.map snd $ S.filter (((==) k) . fst) edges

neighbours :: Key -> Graph a -> S.Set Key
neighbours n g = predecessors n g `S.union` successors n g

predecessorsValues :: Ord a => Key -> Graph a -> [a]
predecessorsValues k graph = map (flip unsafeLookup graph) (S.toList (predecessors k graph))

successorsValues :: Ord a => Key -> Graph a -> [a]
successorsValues k graph = map (flip unsafeLookup graph) (S.toList (successors k graph))

neighboursValues :: Ord a => Key -> Graph a -> [a]
neighboursValues k g = predecessorsValues k g ++ successorsValues k g

fold' :: Ord a => (Key -> b -> b) -> b -> Graph a -> Q.Queue Key -> b
fold' f acc g q = 
    let (curr, newq) = Q.dequeue q
    in case curr of
        Nothing -> acc
        (Just c) -> let succs = S.toList $ successors c g
                    in fold' f (f c acc) (delete c g) (Q.enqueueAll succs newq)

fold :: Ord a => (Key -> b -> b) -> b -> Graph a -> b
fold f acc g = fold' f acc g (Q.enqueue ((head . keys) g) Q.empty)

mapToNumbers :: [a]-> [Integer]
mapToNumbers = map fst . zip [0..]

removeDuplicateEdges :: Edges -> Edges
removeDuplicateEdges = S.foldr (\(x, y) acc -> if not ((y, x) `S.member` acc) && not ((x, y) `S.member` acc) then S.insert (x, y) acc else acc) S.empty

escape :: String -> String
escape = map (\x -> if x == '"' then ' ' else x)

toDotNodes :: Show k => (a -> String) -> [(k, a)] -> String
toDotNodes showF = concatMap (\(x, y) -> show x ++ " [label=\"" ++ (escape . showF) y ++ "\"];\n")

toDotEdges :: Show a => [(a, a)] -> String
toDotEdges = concatMap (\(x, y) -> show x ++ " -- " ++ show y ++ ";\n")

toDot :: (Show a, Ord a) => Graph a -> String
toDot = toDotWith show

toDotWith :: Ord a => (a -> String) -> Graph a -> String
toDotWith showF (Graph nodes edges) = "graph { \n" ++ toDotNodes showF (M.toList nodes) ++ (toDotEdges (S.toList $ removeDuplicateEdges edges)) ++ "}\n"