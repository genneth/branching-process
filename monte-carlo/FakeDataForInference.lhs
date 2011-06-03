> import RandomClone 
>   ( BranchingProcess, progeny_p, lifetime_G_inv
>   , cloneFrom, cellsAliveAt, cloneAt)
> import System (getArgs)
> import Control.Monad.Random (evalRandIO)
> import Control.Monad (replicateM, liftM)
> import Data.List (group, sort, intersperse)

We want to generate some decent fake data for testing the inference engine. We
want to give it: r, gamma, m, lambda and t, and the number of clones, and get
back a grid that can be pasted straight into Matlab for processing by clone-dists. 

> data Parameters = Parameters { r :: !Double
>                              , gamma :: !Double
>                              , m :: !Double
>                              } deriving (Show, Eq, Ord)

We end up having to carry this structure around. Let's do this for the moment
until we get tired and end up monad-ifying:

> data CellType = A !Parameters | B !Parameters | C !Parameters
>   deriving (Show, Eq, Ord)

> instance BranchingProcess CellType where
>   progeny_p (A p) = [ ([A p, A p], r p)
>                     , ([A p, B p], 1 - 2*(r p))
>                     , ([B p, B p], r p)]
>   progeny_p (B p) = [ ([C p], 1.0) ]
>   progeny_p (C p) = [ ([], 1.0) ]
>   lifetime_G_inv (A p) g = -(log (1-g))
>   lifetime_G_inv (B p) g = -(log (1-g)) / gamma p
>   lifetime_G_inv (C p) g = -(log (1-g)) * gamma p / (m p * (1+gamma p))

Output one such table for a given set of values:

> matlabTable r gamma m lambda t tot = do
>   let p = Parameters r gamma m
>       divs = lambda*t
>   counts <- replicateM tot $ liftM countCells $ cloneAt divs (A p)
>   let m = maximum . map fst $ counts
>       n = maximum . map snd $ counts
>   let dist = [[countClones b s counts | s <- [0..n]] | b <- [0..m]]
>   putStr "% "
>   putStrLn $ concat $ intersperse " " $ map show [r, gamma, fromIntegral m, lambda, t, fromIntegral tot]
>   putStrLn $ "ts3(end+1) = " ++ (show t) ++ ";"
>   putStrLn $ "normal{end+1} = ["
>   printTable dist
>   putStrLn ""
>   putStrLn "\t];"
>   putStrLn ""

> main = do
>   sequence_ $ [matlabTable 0.13 1.5 0.82 1.3 t n | t <- [2,5], n<-[100,100,100,100,100,300,300,300,300,300,1000,1000,1000,1000,1000]]
>   sequence_ $ [matlabTable 0.13 1.5 0.82 0.65 t n | t <- [2,5], n<-[100,100,100,100,100,300,300,300,300,300,1000,1000,1000,1000,1000]]
>   sequence_ $ [matlabTable 0.13 1.5 0.82 2.6 t n | t <- [2,5], n<-[100,100,100,100,100,300,300,300,300,300,1000,1000,1000,1000,1000]]
>   sequence_ $ [matlabTable 0.25 1.5 0.82 1.3 t n | t <- [2,5], n<-[100,100,100,100,100,300,300,300,300,300,1000,1000,1000,1000,1000]]
>   sequence_ $ [matlabTable 0.25 1.0 0.82 1.3 t n | t <- [2,5], n<-[100,100,100,100,100,300,300,300,300,300,1000,1000,1000,1000,1000]]

Count the number of basal and suprabasal cells:

> countCells cells = ( length . filter isAB $ cells
>                    , length . filter isC  $ cells)
>   where
>     isAB (A _) = True
>     isAB (B _) = True
>     isAB _     = False
>     isC (C _)  = True
>     isC _      = False

> countClones b s = length . filter (==(b,s))

> printTable rows = sequence_ $ intersperse (putStrLn "") (map printRow rows)
>   where printRow row = putStr "\t" >> (sequence_ $ intersperse (putStr "\t") (map (putStr . show) row))

