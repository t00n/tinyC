module Queue where

import qualified Data.Sequence as S

type Queue a = S.Seq a

empty :: Queue a
empty = S.empty

null :: Queue a -> Bool
null = S.null

enqueue :: a -> Queue a -> Queue a
enqueue = (S.<|)

enqueueAll :: [a] -> Queue a -> Queue a
enqueueAll = flip (foldr enqueue)

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue q = 
    let view = S.viewr q
    in
    case view of
        S.EmptyR -> (Nothing, q)
        (S.:>) newq x -> (Just x, newq)