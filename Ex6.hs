module Ex6
( MultiSet
, joinToMS
, joinToListBag
) where


import Ex1
import Ex2

-- forcing an obligatory namespace qualifier (MS) to imported identifiers
import qualified Data.MultiSet as MS


class MultiSet ms where
    -- constructors
    empty      :: ms a
    singleton  :: a -> ms a
    fromList   :: Ord a => [a] -> ms a
    -- operations
    isEmpty :: Eq a => ms a -> Bool
    mul :: Ord a => a -> ms a -> Int
    toList :: ms a -> [a]
    sumBag :: Ord a => ms a -> ms a -> ms a

-- the types inferred by the compiler for this functions are the ones given by the class definition
instance MultiSet ListBag where
    empty = Ex1.empty 
    singleton = Ex1.singleton
    fromList = Ex1.fromList
    isEmpty = Ex1.isEmpty
    mul = Ex1.mul
    toList = Ex1.toList
    sumBag = Ex1.sumBag

instance MultiSet MS.MultiSet where
    empty = MS.empty
    singleton = MS.singleton
    fromList = MS.fromList

    isEmpty = MS.null
    mul = MS.occur
    toList = MS.toList
    sumBag = MS.union


listbag = LB[(1,2),(2,3),(3,4)]
multiset = MS.fromList [1,1,2,2,2,3,3,3,3]

-- join to MultiSet in one ListBag
joinToListBag :: (Ord a, MultiSet ms2, MultiSet ms1) => ms2 a -> ms1 a -> ListBag a
joinToListBag bag1 bag2 = Ex6.fromList ((Ex6.toList bag1) ++ (Ex6.toList bag2))   

-- join to MultiSet in one MS.MultiSet
joinToMS :: (Ord a, MultiSet ms2, MultiSet ms1) => ms2 a -> ms1 a -> MS.MultiSet a
joinToMS bag1 bag2 = Ex6.fromList ((Ex6.toList bag1) ++ (Ex6.toList bag2)) 