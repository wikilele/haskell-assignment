module Ex1 
( ListBag (LB) 
, wf
, empty
, singleton
, add
, add'
, remove
, fromList
, isEmpty
, mul 
, toList
, sumBag
) where

import Data.List

data ListBag a = LB [(a, Int)] deriving (Show, Eq)

unwrap :: ListBag t -> [(t, Int)]
unwrap (LB bag) = bag

equal :: Eq a => a -> (a, b) -> Bool
equal e bagelem = e == fst(bagelem)

notequal :: Eq a => a -> (a, b) -> Bool
notequal e bagelem = e /= fst(bagelem)

is_in :: Eq a => a -> ListBag a -> Bool
is_in e (LB bag) = foldl (\acc item -> acc || (equal e item)) False bag 


-- WF
wf :: Eq a => ListBag a -> Bool
wf (LB []) = True
wf (LB (x:xs)) = if fst(x) `is_in` (LB xs) then False else wf (LB xs)


-- assuming fst pair isn't in bag
add :: (a, Int) -> ListBag a -> ListBag a
add pair (LB bag) = LB (pair : bag)

remove :: Eq a => a -> ListBag a -> ListBag a
remove a (LB bag) = LB (filter (\pair -> notequal a pair) bag)

-- adds and keeps the LB well formed
add' :: Eq a => (a, Int) -> ListBag a -> ListBag a
add' (a',b') lbag = let m = mul a' lbag
                        lpairs = remove a' lbag
                    in add (a', b' + m) (lpairs)

---- CONSTRUCTORS
empty :: ListBag a
empty = LB []

singleton :: a -> ListBag a
singleton v = LB [(v,1)]

-- sorts the input list and groups in sublists the equal elements 
-- the foldl scans the list of list and uses the sublist length as the element multiplicity
fromList :: Ord a => [a] -> ListBag a
fromList lst =  foldl (\(LB bag) xs -> add (head xs, length xs) (LB bag) ) empty (group $ sort lst)

---- OPERATIONS
isEmpty :: Eq a => ListBag a -> Bool
isEmpty bag = if bag == (LB []) then True else False 

mul :: Eq a => a -> ListBag a -> Int
mul v bag = foldl (\acc item -> if (equal v item) then snd(item) else acc) 0 (unwrap bag)

toList :: ListBag a -> [a]
toList bag = foldl (\xs item -> (replicate (snd item) (fst item)) ++ xs) [] (unwrap bag)

sortb :: Ord b => ListBag b -> [(b, Int)]
sortb bag = sortOn fst (unwrap bag)


merge :: (Num b, Ord a) => [(a, b)] -> [(a, b)] -> [(a, b)]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if (fst x) == (fst y) then (fst x, (snd x) + (snd y)) : (merge xs ys)
                      else if (fst x) > (fst y) then y : (merge (x:xs) ys)
                      else x : ( merge xs (y:ys))


sumBag :: Ord a => ListBag a -> ListBag a -> ListBag a
sumBag bag bag' = LB $ merge (sortb bag) (sortb bag')
