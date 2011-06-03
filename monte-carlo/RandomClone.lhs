> module RandomClone
>   ( BranchingProcess, progeny_p, lifetime_G_inv
>   , CloneTree, cloneFrom, cellsAliveAt, cloneAt
>   ) where

> import Control.Monad.Random (MonadRandom, getRandomR)
> import Control.Monad (liftM)

The plan is to exploit MonadRandom to the hilt and produce a datatype for clones
where one can simply ask for a random clone and it will be produced on demand. 
We will need to keep an eye on laziness; we want to avoid computing beyond what
we need (up to a certain time) and also abort/restart on extinct clones.

First up, everything necessary to describe a multi-type, age-dependent branching
process.

> class BranchingProcess a where
>   initial_p :: [(a, Double)] -- probability of starting clone with type i
>   progeny_p :: a -> [([a], Double)]
>   lifetime_G_inv :: a -> Double -> Double

Test instance:

> data ABHomoeostasis = A | B deriving (Show, Eq, Ord)
> instance BranchingProcess ABHomoeostasis where
>   initial_p = [(A,1.0),(B,0.0)]
>   progeny_p A = [([A,A], r1), ([A,B],1-r1-r2), ([B,B],r2)]
>     where r1 = 0.13
>           r2 = 0.13
>   progeny_p B = [([],1.0)]
>   lifetime_G_inv _ g = -log (1-g)

The clone itself:

> data CloneTree a = Cell !a !Double [CloneTree a] deriving Show

Now, to generate a random clone, we need to be able to randomly pick the progeny
of a cell:

> progeny :: (BranchingProcess a, MonadRandom r) => a -> r [a]
> progeny i = oneOf $ progeny_p i

where we need a helper function:

> oneOf :: (MonadRandom r) => [(a, Double)] -> r a
> oneOf choices = do
>     x <- getRandomR (0.0, 1.0)
>     return $ choose x choices
>   where
>     choose y ((c,p):cs) 
>       | y < p     = c
>       | otherwise = choose (y-p) cs

> cloneFrom :: (BranchingProcess a, MonadRandom r) => a -> r (CloneTree a)
> cloneFrom i = do
>     daughters <- progeny i
>     subclones <- mapM cloneFrom daughters
>     lifetime <- liftM (lifetime_G_inv i) $ getRandomR (0.0, 1.0)
>     return $ Cell i lifetime subclones

> clone :: (BranchingProcess a, MonadRandom r) => r (CloneTree a)
> clone = do
>   initial <- oneOf initial_p
>   cloneFrom initial

For a given clone, we might want to ask which cells are alive at a time t.
Should be a simple recursive program, but we do need to make sure that it is
sufficiently lazy, as to avoid having to evaluate a whole clone (including way
past time t) to get to the answer.

> cellsAliveAt t (Cell i l ds)
>   | t < l     = [i]
>   | otherwise = concatMap (cellsAliveAt (t-l)) ds

Or we can try and be more direct:

To get the clone size at time t, we simply have to recurse:

> cloneAt :: (BranchingProcess a, MonadRandom r) => Double -> a -> r [a]
> cloneAt t i = do
>   lifetime <- liftM (lifetime_G_inv i) $ getRandomR (0.0, 1.0)
>   if t < lifetime
>     then return [i]
>     else do
>       daughters <- oneOf $ progeny_p i
>       progeny <- mapM (cloneAt (t-lifetime)) daughters
>       return $ concat progeny


