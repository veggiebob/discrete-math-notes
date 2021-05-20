module Counting where

orderedInts = 0:concat [[i,-i] | i<-[1..]]

-- get an infinite list of ordered pairs that originate from the center (for natural numbers)
orderedPairInf :: [(Int, Int)]
orderedPairInf = concat [ [ (x, i-x) | x <- [0..i] ] | i <- [0..]]
orderedNatPairInf = map (\(x,y) -> (x+1, y+1)) orderedPairInf

-- get an infinite list of ordered triplets that originate from the center (for natural numbers)
orderedNatTripleInf :: [(Int, Int, Int)]
orderedNatTripleInf = concat . concat $ [ [ [ (i+2-z-y, y, z) | y<-[1..i-z+1] ] | z<-[1..i] ] | i<-[1..] ]

orderedTripleInf = concat . concat $ [ [ [ (i-z-y, y, z) | y<-[0..i-z] ] | z<-[0..i] ] | i<-[0..] ]

orderedNVecInf' :: Int -> [Int] -> [[Int]]
orderedNVecInf' 0 m = [ [d, lm-d] | d<-[0..lm] ] where lm = foldl (-) (head m) (tail m)
orderedNVecInf' n m = concat [ map (d:) $ orderedNVecInf' (n-1) (m++[d]) | d<-[0..lm] ]
    where lm = foldl (-) (head m) (tail m)
orderedNVecInf :: Int -> [[Int]]
orderedNVecInf n | n < 2 = map (\x -> [x]) orderedInts
                 | otherwise = concat [ orderedNVecInf' dim [i] | i<-[0..]] where dim = n-2

orderedNVecNatInf n = map (map (+1)) $ orderedNVecInf n

orderedCoordinatePlane :: [(Int, Int)]
orderedCoordinatePlane = (0,0):interleaves [xp, xpyp, yp, xnyp, xn, xnyn, yn, xpyn]
    where   xp = [ ( x, 0) | x <- [1..] ]
            xn = [ (-x, 0) | x <- [1..] ]
            yp = [ (0,  y) | y <- [1..] ]
            yn = [ (0, -y) | y <- [1..] ]

            xpyp = orderedNatPairInf
            xnyp = map (\(x, y) -> (-x,  y)) xpyp
            xnyn = map (\(x, y) -> (-x, -y)) xpyp
            xpyn = map (\(x, y) -> ( x, -y)) xpyp

interleave :: [a] -> [a] -> [a]
interleave []     []     = []
interleave [x]    (y:ys) = x:y:ys
interleave (x:xs) [y]    = x:y:xs
interleave (x:xs) (y:ys) = x:y:interleave xs ys

interleaves :: [[a]] -> [a]
interleaves [] = []
interleaves xss = map head fss ++ interleaves (map tail fss)
    where fss = filter (not . null) xss


main :: IO ()
main = return ()