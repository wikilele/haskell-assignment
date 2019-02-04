module Ex5
( returnLB
, bindLB
, myfun
) where

import Ex1
import Ex2

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

returnLB :: a -> ListBag a
returnLB a = singleton a

-- just a simple function that can be used with the bind operator
myfun :: Num a => a -> ListBag a
myfun v = LB [(v + 42,1)]

-- takes a list of ListBag and concats them in a single list
-- well formed not guarenteed
concatLB :: [ListBag t] -> [(t, Int)]
concatLB [] = []
concatLB ((LB x):xs ) = x ++ concatLB xs

bindLB :: ListBag b -> (b -> ListBag a) -> ListBag a
bindLB (LB xs) f = LB $ concatLB $ map (f . fst ) xs


-- "Every Monad is a functor" (d)
-- In order to make ListBag an istance of Monad I need to relax the 'well formed constraint'
-- If we strictly want only well formed LB we can't have a proper bind function
instance Functor ListBag where
    -- liftM :: Monad m => (a1 -> r) -> m a1 -> m r   (a)
    -- liftM f xs = xs >>= (return . f)
    --   take a container full of a's, to each, apply f,
    --   put the resulting value of type b in a new container,
    fmap = liftM

-- Monad proposal (AMP), which was then implemented in GHC 7.10. (c)
-- Applicative becomes a superclass of Monad
-- Control.Applicative module describes a structure intermediate between a functor and a monad.
-- Compared with monads, this interface lacks the full power of the binding operation >>= [...]
instance Applicative ListBag where
    pure = returnLB
    -- ap :: Monad m => m (a -> b) -> m a -> m b  (a)
    (<*>) = ap

instance Monad ListBag where
    return = pure
    (>>=) m f = bindLB m f



-- Some links I used to do some research about this topic
-- (a)  http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html
-- (b)  https://stackoverflow.com/questions/31652475/defining-a-new-monad-in-haskell-raises-no-instance-for-applicative
-- (c)  https://wiki.haskell.org/Functor-Applicative-Monad_Proposal
-- (d)  https://wiki.haskell.org/Monads_as_containers 
-- (e)  http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html