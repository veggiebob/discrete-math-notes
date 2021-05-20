module Group where
import NumberTheory
import Data.List
import Data.Foldable
import Set
-- cartesian product
cp s = [ [ (x, y) | y <- s ] | x <- s ]

newtype Table a = Table [[a]]

pad :: Int -> String -> String
pad i s | length s < i = pad i (' ':s)
        | otherwise = s

instance Foldable Table where
    foldr f z (Table []) = z
    foldr f z (Table (r:rs)) = foldr f (foldr f z r) (Table rs)

instance Functor Table where
    fmap f (Table t) = Table $ map (map f) t

instance (Show a) => Show (Table a) where
    show (Table []) = ""
    show tbl@(Table (row:table)) = (foldr (++) "" . map (pad (mxlen + 2) . show) $ row) ++ "\n" ++ show (Table table)
        where l = Data.Foldable.toList tbl
              mxlen = foldr max 0 (map (length . show) l)

instance (Show a, Eq a) => Show (Group a) where
    show gp = "A Group (G, *) \nwith a set: " ++ show grpSet ++ "\nwith a cayley table:\n" ++ tbl ++ "identity is " ++ show ident ++ "\norder is " ++ show order
        where tbl = show $ cayley gp
              ident = identity gp
              order = orderOfGroup gp
              grpSet = getSet gp

data Group a = Group [a] (a -> a -> a)

(<<>>) :: Eq a => Group a -> a -> Group a
grp@(Group set f) <<>> x | x `notElem` set = error "not an element of the set in the group!"
                         | otherwise = Group cset f
    where   cset = helper [x]
            helper curr | c `elem` curr = curr
                        | otherwise = helper $ curr ++ [c]
                        where c = (last curr) `f` x


-- this and the next function are identical except for their return values
findGeneratorSubgroups :: Eq a => Group a -> [(Group a, a)]
findGeneratorSubgroups grp@(Group set f) = filter (\(a,b) -> a `subGroup` grp) $
                                            map (\x -> (grp<<>>x, x)) set

findSubgroupGenerators :: Eq a => Group a -> [a]
findSubgroupGenerators grp@(Group set _) = map snd . filter ((`properSubset` set) . fst) . map (\x -> (getSet . (grp<<>>) $ x, x)) $ set



findGroupGenerators :: Eq a => Group a -> [a]
findGroupGenerators grp@(Group set _) = map snd .
                                        filter ((`setEquals` set) . fst) .
                                        map (\x -> (getSet . (grp<<>>) $ x, x)) $
                                        set

-- assume groups have the same order
--checkIsomorphism g1@(Group set1 f) g2@(Group set2 g) tbl = and . map (checkIsomorphic . curry)
--    $ map (\(x,y) -> (set1!!x, set2!!y))
--    $ cp [0..orderOfGroup g1 - 1]
--    where checkIsomorphic a b =

-- WARNING: assumes groups have the same operation!!
subGroup :: Eq a => Group a -> Group a -> Bool
a `subGroup` b = (getSet a) `properSubset` (getSet b)

cyclic gp@(Group set _) = any (\x -> (getSet $ gp<<>>x) `setEquals` set) set

generatedIn :: Eq a => a -> Group a -> Group a
a `generatedIn` grp = grp <<>> a

groupU :: Int -> Group Int
groupU n = let set = filter (coprime n) [1..n]
           in Group set (\x y -> x * y `mod` n)

groupZ :: Int -> Group Int
groupZ n = Group [0..n-1] (\x y -> (x + y) `mod` n)

getOperation (Group _ op) = op
getSet (Group set _) = set

cayley :: Group a -> Table a
cayley gp = fmap (unpair f) . Table . cp $ getSet gp
    where unpair f (a, b) = f a b
          f = getOperation gp

isIdentity :: Eq a => Group a -> a -> Bool
isIdentity (Group set f) e | e `notElem` set = error "Group.isIdentity: identity element given is not in the group's set!"
                           | otherwise = and $ map identityEquality set
    where identityEquality x = (e `f` x == x) && (x `f` e == x)

identity :: Eq a => Group a -> a
identity gp@(Group set f) | length ident > 1 = error "Group.identity: more than one identity in the group! not a group!"
                             | otherwise = head ident
                             where ident = filter (isIdentity gp) set

inverse :: Eq a => Group a -> a -> a
inverse gp@(Group set f) a | length matches > 1 = error "Group.inverse: unique inverse property not satisfied! not a group!"
                              | a `notElem` set = error "Group.inverse: the element is not in the group's set!"
                              | otherwise = head matches
                              where matches = filter ((==e) . f a) set
                                    e = identity gp

center :: Eq a => Group a -> [a]
center gp@(Group set f) = filter commutative set
    where commutative a = and $ map (\b -> a `f` b == b `f` a) set

centralizers :: Eq a => Group a -> a -> [a]
centralizers gp@(Group set f) a | a `notElem` set = error "Group.centralizers: element is not in the group's set!"
                                | otherwise = czs
                                where czs = filter (\x -> a `f` x == x `f` a) set

orderOfGroup :: Group a -> Int
orderOfGroup = length . getSet

orderOfElem :: Eq a => Group a -> a -> Int
orderOfElem gp@(Group _ f) x = helper f x (identity gp) -- save time on identity calculation
    where helper f c e | c == e = 1
                         | otherwise = 1 + helper f (c `f` x) e

unique :: Eq a => Table a -> [a]
unique (Table t) = nub . concat $ t

main :: IO ()
main = putStrLn "hello world"
