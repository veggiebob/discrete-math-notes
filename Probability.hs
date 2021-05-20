module Probability where
import Set
import Data.List
data Tree a = Node a [Tree a] | Leaf a deriving Show

-- a tree that needs cleaning (remove meaningless parts)
data UncleanTree a = Unclean a [UncleanTree a] | Clean (Tree a) | DeadLeaf

instance Functor Tree where
    fmap f (Node x xs) = Node (f x) (fmap (f<$>) xs)
    fmap f (Leaf x) = Leaf $ f x

clean :: UncleanTree a -> Tree a
clean (Unclean x xs)
    | length cleaned == 0 = Leaf x
    | otherwise = Node x cleaned
    where   cleaned = map clean . filter (not . empty) $ xs
            empty DeadLeaf  = True
            empty _         = False
clean (Clean t) = t
clean DeadLeaf = error "Probability.clean encountered a DeadLeaf before it was removed when cleaning an UncleanTree."

leaves :: Tree a -> [Tree a]
leaves tree = leaves' [] tree
    where   leaves' o (Node x xs) = o ++ (concat . map leaves $ xs)
            leaves' o lf = o ++ [lf]

buildSumTree :: Monoid a => Tree a -> Tree a
buildSumTree = buildSumTree' mempty

buildSumTree' :: Monoid a => a -> Tree a -> Tree a
buildSumTree' i (Node x xs) = Node c (map (buildSumTree' c) xs)
    where c = mappend i x
buildSumTree' i (Leaf x) = Leaf $ mappend i x

newtype Rule a = Rule (a -> Bool)

-- apply a rule to Tree a
ruledTree :: Rule a -> Tree a -> UncleanTree a
ruledTree r@(Rule rule) (Node x xs)
    | rule x = if length result > 0 then Unclean x result else Clean $ Leaf x
    | otherwise = DeadLeaf
    where result = map (ruledTree r) xs
ruledTree (Rule rule) leaf@(Leaf x) = if rule x then Clean leaf else DeadLeaf

hasValidLeaf :: Rule a -> Tree a -> Bool
hasValidLeaf (Rule rule) (Leaf x) = rule x
hasValidLeaf r@(Rule rule) (Node x xs) = or . map (hasValidLeaf r) $ xs

leafRuledTree :: Rule a -> Tree a -> UncleanTree a
leafRuledTree rule node@(Node x xs)
    | hasValidLeaf rule node = Unclean x (map (leafRuledTree rule) . filter (hasValidLeaf rule) $ xs)
    | otherwise = DeadLeaf
leafRuledTree (Rule rule) leaf@(Leaf x) = if rule x then Clean leaf else DeadLeaf

newtype Generate a = Generate (a -> [a])

generateTree :: Generate a -> a -> Tree a
generateTree gf@(Generate g) x
    | length gen == 0 = Leaf x
    | otherwise = Node x (map (generateTree gf) gen)
        where gen = g x

generateNoRootTree :: Monoid a => Generate a -> Tree a
generateNoRootTree = (`generateTree` mempty)

-- Convenience --

-- rule to not repeat
noRepeat :: Eq a => [a] -> Bool
noRepeat x = null $ x \\ nub x

-- generator to generate an infinite tree
allOptionsInfinite set = Generate $ \_ -> set

-- generate an infinite tree using the set
allOptionsInfiniteTree set = generateNoRootTree $ allOptionsInfinite set

permutedTree :: [a] -> Tree [a]
permutedTree xs =
    clean .
    ruledTree (Rule $ \x -> length x <= length xs) .
    buildSumTree .
    generateNoRootTree
    $ Generate $ \s -> map (\x->[x]) xs


-- homework

keypad = [
    (1, "1QZ"),
    (2, "2ABC"),
    (3, "3DEF"),
    (4, "4GHI"),
    (5, "5JKL"),
    (6, "6MNO"),
    (7, "7PRS"),
    (8, "8TUV"),
    (9, "9WXY"),
    (0, "0")]

reverseKeypad :: Char -> Int
reverseKeypad c = fst . head . filter ((c `elem`) . snd) $ keypad
allKeys = concat . map snd $ keypad

{-
ruledTree (Rule $ \x -> noRepeat (map reverseKeypad x)) -- no repeat digit
clean .
ruledTree (Rule $ \x -> length x <= 4) .
buildSumTree .
generateNoRootTree $ Generate $ map (\x->[x]) allKeys
-}