module NumberTheory where
import Data.List (nub)

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime = (==2) . length . factors

coprime :: Int -> Int -> Bool
coprime x y = null $ intersection (tail $ factors x) (tail $ factors y) -- tail to remove 1 as a factor

union :: Eq a => [a] -> [a] -> [a]
union xs = nub . (xs++)

intersection :: Eq a => [a] -> [a] -> [a]
intersection a b = union (filter (`elem` b) a) (filter (`elem` a) b)