module Ex2
( mapLB
) where

import Ex1

instance Foldable ListBag where
    -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
    foldr f acc (LB bag) = foldr (\pair acc -> f (fst pair) acc ) acc bag

mapLB :: (a1 -> a) -> ListBag a1 -> ListBag a
mapLB f (LB bag) = LB ( map (\e -> ( f $ fst e ,  snd e)) bag )


-- Since the mapLB just applies a function to all the element of the ListBag
-- there is no guarantee that the returned LB will be well formed.
-- This is the case of applying a costant function for example ( see the file testEx2_mapLB.hs).
-- From this facts follows that if we build a fmap with the mapLB we are going against the semantic
-- of the fmap function because we return an improper (not well formed) List Bag

-- Anyway this is the implementation breaking the 'well fromed' constraint
-- instance Functor ListBag where
    -- fmap f lbag = mapLB f lbag 
