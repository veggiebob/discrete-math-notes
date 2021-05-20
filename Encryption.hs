module Encryption (
    createEncryption,
    encryptWith,
    decryptWith,
    encryptString,
    decryptToString,
    homeworkEncrypt,
    homeworkDecrypt
    ) where

import Data.List (intersect)
import Data.Char (ord, chr)

-- remember to use Integer for near-infinite precision
--                           p       q       e       d
data Encryption = Encryption Integer Integer Integer Integer

instance Show Encryption where
    show (Encryption p q e d) = "Encryption { p = " ++ show p
                                    ++ ", q = " ++ show q ++ ", e = " ++ show e ++
                                    ", d = " ++ show d ++ " }"

createEncryption p q = Encryption p q e d
    where e = head . filter (coprime tocient) $ [2..tocient]
          d = e `inverseMod` tocient
          tocient = tocient

-- helper methods
factors n = [x | x <- [1..(ceiling . sqrt) n], n `mod` x == 0]
coprime x y = intersect (factors x) (factors y) == [1]
congruentOver n x y = (x `mod` n) == (y `mod` n)

-- naive inverseMod, only finds positive solutions
x `inverseMod` y = helper 1 x y where
    helper n a b = if congruentOver y 1 (x * n) then n else helper (n + 1) x y

-- to calculate d
-- d = e `inverseMod` ((p - 1) * (q - 1))
-- and if you don't care what e is
-- e = head . filter (coprime ((p - 1) * (q - 1))) $ [2..]

encryptWith (Encryption p q e d) m = (m ^ e) `mod` (p * q)
decryptWith (Encryption p q e d) c = (c ^ d) `mod` (p * q)

{-
for a certain integer x and a certain Encryption known as En,
the function (decryptWith En . encryptWith En) is an identity
function as long as n < p * q
-}

encryptString en = map (encryptWith en . toInteger . ord)
decryptToString en = map (chr . fromInteger . decryptWith en)

-- double check that numbers cannot be bigger than p * q I think
encryptionLimit en = helper 5
    where helper n | (decryptWith en . encryptWith en $ n) == n = helper (n+1)
                   | otherwise = n

--numberToLetterTable :: Integral a => [(a, Char)]
numberToLetterTable = zip [1..] ['A'..'Z']

--letterToNumberTable :: Integral a => [(Char, a)]
letterToNumberTable = zip ['A'..'Z'] [1..]

numberToLetter = (`lookup` numberToLetterTable)
letterToNumber = (`lookup` letterToNumberTable)


--homeworkEncrypt :: Integral b => Encryption -> [a] -> [b]
homeworkEncrypt en = map $
        encryptWith en .
        unsafeUnjust .
        letterToNumber
        where unsafeUnjust (Just x) = x
              unsafeUnjust Nothing = -1

--homeworkDecrypt :: Integral b => Encryption -> [b] -> [a]
homeworkDecrypt en = map $
    numberToLetter .
    fromInteger .
    decryptWith en

