module Demo where

factors n = [ x | x <- [1..n], n `mod` x == 0]

prime = (==2) . length . factors

primeFactorization n | prime n = [n]
                     | otherwise = p:primeFactorization (n `div` p)
    where p = head . filter prime . tail . factors $ n

validatePrimeFactorization n = (==n) . foldr (*) 1