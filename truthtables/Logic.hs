module Logic (createTruthTable, prettyPrintTruthTable, serializeLogic) where
import Data.Maybe (fromJust)
import Data.List (intercalate)

-- misc
surroundSpace :: String -> String
surroundSpace s = " " ++ s ++ " "
trim s = takeWhile (/=' ') . drop n $ s
    where n = length (takeWhile (==' ') s)

-- stolen from https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

-- logic boilerplate
lnot a = not a
lor a b = a || b
land a b = a && b
lifthen a b = lnot a `lor` b

-- symbols
_ ~~ b = not b -- yes, this is unary, but there are no unary operators in Haskell

a /\ b = a `land` b
a \/ b = a `lor` b

a ==> b = a `lifthen` b
a <=> b = (a ==> b) `land` (b ==> a)

stringOperatorTable :: String -> Maybe (Bool -> Bool -> Bool)
stringOperatorTable s | s == "^" = Just (/\)
                      | s == "v" = Just (\/)
                      | s == "->" = Just (==>)
                      | s == "<=>" = Just (<=>)
                      | s == "~" = Just (~~) -- dummy on left side will be "_"
                      | otherwise =  Nothing -- error $ "stringOperatorTable: no function for '" ++ s ++ "'!"

-- expressions
data Expression a = Variable Char | Evaluatable (Expression a) String (Expression a)
type Premis = Bool
type Conclusion = Bool
valid :: [Premis] -> Conclusion -> Bool
valid premises conclusion = and premises /\ conclusion == False

exprLeft :: Expression a -> Expression a
exprLeft (Evaluatable x f y) = x
exprLeft x = x

exprRight :: Expression a -> Expression a
exprRight (Evaluatable x f y) = y
exprRight x = x

exprEvaluator :: Expression a -> (Bool -> Bool -> Bool)
exprEvaluator (Evaluatable x f y) = fromJust (stringOperatorTable f)

collectExpressions :: Expression a -> [Expression a]
collectExpressions (Variable x) = []
collectExpressions e@(Evaluatable x f y) = collectExpressions x ++ collectExpressions y ++ [e]

collectVariables :: Expression a -> [Char]
collectVariables (Variable x) | x == '_' = []
                              | otherwise = [x]
collectVariables (Evaluatable x f y) = removeDuplicates (collectVariables x ++ collectVariables y)

instance Show (Expression a) where
    show (Evaluatable x f y) | f == "~" = "~" ++ show y
                             | otherwise = "(" ++ show x ++ surroundSpace f ++ show y ++ ")"
    show (Variable x) = [x]

type InputSet = [(Char, Bool)]
type InputTable = [InputSet]
type OutputSet = [(String, Bool)] -- a row on a truth table
type OutputTable = [OutputSet]

cvtInput2Output :: InputTable -> OutputTable
cvtInput2Output i = [[([c], v) | (c, v) <- k] | k <- i]

zipList :: [[a]] -> [[a]] -> [[a]]
zipList = zipWith (\x y -> x ++ y)

doubleElements :: [a] -> [a]
doubleElements [] = []
doubleElements (x:xs) = x:x:doubleElements xs

multiplyList :: Int -> [a] -> [a]
multiplyList 0 _  = []
multiplyList n xs
    | n > 0 = xs ++ multiplyList (n-1) xs
    | n < 0 = error "multiplyList: n is less than 0"

makeSingle :: Char -> InputTable
makeSingle c = [
                [(c, False)],
                [(c,  True)]
               ]

makeInputTable :: [Char] -> InputTable
makeInputTable [x] = makeSingle x
makeInputTable (x:xs) = zipList a (doubleElements t)
    where a = multiplyList (length t) (makeSingle x); t = makeInputTable xs

evaluate :: InputSet -> Expression Bool -> Bool
evaluate vs (Evaluatable x f y) = fromJust (stringOperatorTable f) (evaluate vs x) (evaluate vs y)
evaluate vs (Variable x) = fromJust (lookup x vs)

createTruthTable :: Expression Bool -> OutputTable
createTruthTable e = [((cvtInput2Output inpTable)!!i) ++ [(show eval, evaluate (inpTable!!i) eval) | eval <- evals] | i <- [0..n]]
    where vars = collectVariables e; evals = collectExpressions e; inpTable = makeInputTable vars; n = length inpTable - 1


cap :: Int -> String -> String
cap n s | length s < n = cap n (s ++ " ")
        | otherwise = s
showBool :: Bool -> String
showBool True = "T"
showBool False = "F"
prettyPrintTruthTable :: OutputTable -> String
prettyPrintTruthTable table = header ++ "\n" ++ (intercalate "\n" . map (intercalate sep . map (\(l, x) -> cap l . showBool . snd $ x) . zip headLens) $ table)
    where header = intercalate sep . map (\(x, y) -> cap x . fst $ y) . zip headLens . head $ table
          sp = 1 + (foldr max 0 . map (length . fst) . head $ table)
          sep = " | "
          headLens = map ((+1) . length . fst) . head $ table

{-
tests
main :: IO ()
--main = putStrLn . show $ makeInputTable "pqr"
--main = putStrLn . concat . map show . collectExpressions $ Evaluatable (Variable 'p') "^" (Evaluatable (Variable 'q') "->" (Variable 'r'))
--main = putStrLn . show . collectVariables $ Evaluatable (Variable 'r') "^" (Evaluatable (Variable 'q') "->" (Variable 'r'))
longTest = Evaluatable (Evaluatable (Variable '_') "~" (Evaluatable (Variable 'z') "<=>" (Variable 'x'))) "^" (Evaluatable (Variable 'q') "->" (Variable 'r'))
simpTest = Evaluatable (Variable 'r') "^" (Evaluatable (Variable 'q') "->" (Variable 'r'))
ezTest = Evaluatable (Variable 'p') "^" (Variable 'q')
main = putStrLn . prettyPrintTruthTable . createTruthTable $ simpTest
--main = putStrLn . show . cvtInput2Output $ makeInputTable "pq"
-}

-- Parsing functions

data Token = Letter Char | OpenParen | CloseParen | Operator String | Empty

instance Show Token where
    show (Letter a) = "Letter " ++ [a]
    show OpenParen = "("
    show CloseParen = ")"
    show Empty = " "
    show (Operator str) = "Operator " ++ str

getToken :: String -> (Token, Int)
getToken s@(c:cs) | c == '(' = (OpenParen, 1)
                  | c == ')' = (CloseParen, 1)
                  | c == ' ' = (Empty, 1)
                  | inop = (Operator op, length op)
                  | c `elem` ('_':['a'..'z']) = (Letter c, 1)
                  | otherwise = (Empty, 1)
                  where nop = filter (inTable . stringOperatorTable) [take n s | n<-[0..(length s - 1)]]
                        inop = length nop > 0
                        op = head nop
                        inside xs x = x `elem` xs
                        inTable Nothing = False
                        inTable _ = True

parse :: String -> [Token]
parse s = parseHelper s []
        where parseHelper [] o = o
              parseHelper s xs = parseHelper (drop i s) (if isEmpty t then xs else (xs ++ [t]))
                where (t, i) = getToken s
                      isEmpty Empty = True
                      isEmpty _     = False

matchParen :: [Token] -> [Token]
matchParen ts = helper ts 1 (-1)
    where helper tokens i net | net == 0 = take i tokens
                              | i >= length tokens = error "no matching parenthesis"
                              | otherwise = helper tokens (i+1) (net + result c)
                              where c = tokens!!i
                                    result OpenParen  = -1
                                    result CloseParen = 1
                                    result _          = 0

simplify :: [Token] -> Expression Bool

-- ew, desugaring
simplify ts@(Operator "~":xs) = simplify (OpenParen:Letter '_':ts ++ [CloseParen]) -- in this case, not simplifying
simplify [Letter c] = Variable c
simplify [Empty] = error "empty"
simplify (Letter x:Operator o:Letter y:[]) = Evaluatable (Variable x) (o) (Variable y)
-- left associativity
simplify (Letter x:Operator o:Letter y:Operator op:xs) = Evaluatable (simplify expr) op (simplify xs)
                                                       where expr = Letter x:Operator o:Letter y:[]
simplify (Letter x:Operator o:OpenParen:xs) = Evaluatable (Variable x) (o) (simplify (OpenParen:xs))
simplify ts@(OpenParen:xs) | isCloseParen (last xs) && length expr == length ts = simplify (tail . init $ expr)
                           | length expr < length ts = Evaluatable (simplify expr) op (simplify (tail end))
                           | otherwise = simplify expr
                           where expr = matchParen ts
                                 end = drop (length expr) ts
                                 (Operator op) = head end
                                 isCloseParen CloseParen = True
                                 isCloseParen _          = False
-- any other case
simplify _ = error "bad expression"

desugar :: String -> String
desugar s = concat [helper c | c <- s]
            where helper c = [c]
--                  helper '~' = "_~"
--                  helper c   = [c]

-- wrapper
serializeLogic :: String -> Expression Bool
serializeLogic = simplify . parse . desugar

-- Tests
{-
main :: IO ()

main = do
    putStrLn "enter your input:"
    input <- getLine
    putStrLn . show $ (simplify . parse) input

--testTokens = [OpenParen, Letter 'p', Operator "^", OpenParen, Letter 'r', Operator "->", Letter 'p', CloseParen, CloseParen, Operator "v", Letter 'r']
--main = putStrLn . show $ simplify testTokens
-}