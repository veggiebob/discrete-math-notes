module Set where
import Data.List
import Data.Function (on)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd (x:xs) ys = map ((,) x) ys ++ cartProd xs ys
cartProd [] ys = []

isSet :: Eq a => [a] -> Bool
isSet set = length (nub set) == length set

subset :: Eq a => [a] -> [a] -> Bool
a `subset` b = and $ map (`elem` b) a

properSubset :: Eq a => [a] -> [a] -> Bool
a `properSubset` b = a `subset` b && length a < length b

setEquals :: Eq a => [a] -> [a] -> Bool
a `setEquals` b = a `subset` b && b `subset` a

--xs `_remove` x = filter (/=x) xs
--_powerset xs len  | len == 0 = [[]]
--                  | len == 1 = ss
--                  | otherwise = ([]:) . sort . nub . map sort . (++ss) . (xs:) . concat . concat $
--                            [
--                            [
--                            [ y:z | z <- _powerset (xs `_remove` y) le ] | y<-xs ] | le <- [0..len-1]]
--                  where ss = map (\x -> [x]) xs
--powerset xs = _powerset xs (length xs + 2)

-- solution adapted from jan
powerSet xs = sortBy (compare `on` length) . reverse $ powerSet' (reverse xs) []
  where
  powerSet' [] ys = [ys]
  powerSet' (x:xs) ys = powerSet' xs (x:ys) ++ powerSet' xs ys
