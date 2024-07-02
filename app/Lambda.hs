module Lambda where

import Control.Monad
import Data.Char (isAlpha, isDigit)
import Data.List (elemIndex, nub)
import Data.Maybe

maximum' :: (Ord a, Num a) => [a] -> a
maximum' [] = 0
maximum' s = maximum s

delete :: (Eq a) => a -> [a] -> [a]
delete a = filter (/= a)

add :: (Eq a) => a -> [a] -> [a]
add a as = nub $ a : as

union :: (Eq a) => [a] -> [a] -> [a]
union as bs = nub $ as ++ bs

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection as = filter (`elem` as)

prefixLength :: (Eq a) => [a] -> [a] -> Int
prefixLength [] _ = 0
prefixLength _ [] = 0
prefixLength (x : xs) (y : ys)
  | x == y = 1 + prefixLength xs ys
  | otherwise = 0

checkCondition :: (a -> Bool) -> a -> Maybe a
checkCondition f a = if f a then Just a else Nothing

getCombinedTerms :: String -> [String]
getCombinedTerms [] = []
getCombinedTerms (var : rest)
  | null rest = [[var]]
  | isAlpha var = (var : take n1 rest) : getCombinedTerms (drop n1 rest)
  | var == '(' = (var : take n2 rest) : getCombinedTerms (drop n2 rest)
  | var == '[' = (var : take n3 rest) : getCombinedTerms (drop n3 rest)
  | otherwise = [var] : getCombinedTerms rest
  where
    findAlpha :: String -> Int
    findAlpha [] = 0
    findAlpha (a : as)
      | a == '(' = 0
      | a == '[' = 0
      | isAlpha a = 0
      | otherwise = 1 + findAlpha as
    n1 :: Int
    n1 = findAlpha rest
    findClosingParen :: String -> Int -> Int
    findClosingParen [] _ = 0
    findClosingParen ('(' : rest') step = 1 + findClosingParen rest' (step + 1)
    findClosingParen (')' : rest') step
      | step == 0 = 1
      | otherwise = 1 + findClosingParen rest' (step - 1)
    findClosingParen (_ : rest') step = 1 + findClosingParen rest' step
    n2 :: Int
    n2 = findClosingParen rest 0
    findClosingBracket :: String -> Int -> Int
    findClosingBracket [] _ = 0
    findClosingBracket ('[' : rest') step = 1 + findClosingBracket rest' (step + 1)
    findClosingBracket (']' : rest') step
      | step == 0 = 1
      | otherwise = 1 + findClosingBracket rest' (step - 1)
    findClosingBracket (_ : rest') step = 1 + findClosingBracket rest' step
    n3 :: Int
    n3 = findClosingBracket rest 0

raise :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
raise f ma mb = mb >>= raise' f ma
  where
    raise' :: (Monad m) => (a -> b -> c) -> m a -> b -> m c
    raise' f' ma' b' = fmap (`f'` b') ma'

uncurry' :: (a -> b -> c -> d) -> a -> (b, c) -> d
uncurry' f a = uncurry (f a)

-- ┌───────────────────────────┐
-- │ the lambda calculus model │
-- └───────────────────────────┘

varSetFormal :: [String]
varSetFormal = ['v' : replicate n '\'' | n <- [0 ..]]

varSet :: [Char]
varSet = ['x', 'y', 'z', 'w', 'u', 't', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'p', 'q', 'n', 'm', 'k']

type Variable = Int

data Lambda = Var Variable | Abst Variable Lambda | Appl Lambda Lambda deriving (Read, Show, Eq)

combinatorI :: Lambda
combinatorI = parseJust "\\x.x"

combinatorK :: Lambda
combinatorK = parseJust "\\x,y.x"
true :: Lambda
true = combinatorK

combinatorK' :: Lambda
combinatorK' = parseJust "\\x,y.y"
false :: Lambda
false = combinatorK'

combinatorS :: Lambda
combinatorS = parseJust "\\x,y,z.xz(yz)"

combinatorY :: Lambda
combinatorY = parseJust "\\f.(\\x.f(xx))(\\x.f(xx))"

combinatorOmega :: Lambda
combinatorOmega = parseJust "(\\x.xx)(\\x.xx)"

church :: Int -> Lambda
church 0 = adjustBoundVars $ Abst 0 combinatorI
church n = adjustBoundVars $ Abst 0 $ Abst 1 $ Appl (Var 0) $ Appl (Appl (church (n-1)) (Var 0)) (Var 1)
zeroChurch :: Lambda
zeroChurch = parseJust "\\x.x((\\y,z.y)(\\y,z.z))(\\y,z.y)"
succChurch :: Lambda
succChurch = parseJust "\\f,x,y.fx(xy)"
prevChurch :: Lambda
prevChurch = parseJust "\\x,y,z.x(\\p,q.q(py))((\\u,w.u)z)(\\t.t)"

pair :: Lambda -> Lambda -> Lambda
pair l1 l2 = Abst n (Appl (Appl (Var n) l1) l2)
  where
    n = 1 + max (maximum' $ freeVarSet l1) (maximum' $ freeVarSet l2)

barend :: Int -> Lambda
barend 0 = combinatorI
barend n = pair false (barend $ n - 1)
zeroBarend :: Lambda
zeroBarend = parseJust "\\x.x(\\y,z.y)"
succBarend :: Lambda
succBarend = parseJust "\\f,x.x(\\y,z.z)f"
prevBarend :: Lambda
prevBarend = parseJust "\\f.f(\\x,y.y)"

-- ┌──────────────────────┐
-- │ Parsing lambda terms │
-- └──────────────────────┘

preprocess :: String -> String
preprocess [] = []
preprocess ('I' : rest) = "(" ++ unparse combinatorI ++ ")" ++ preprocess rest
preprocess ('K' : '*' : rest) = "(" ++ unparse combinatorK' ++ ")" ++ preprocess rest
preprocess ('f':'a':'l':'s':'e':rest) = preprocess $ 'K' : '*' : rest
preprocess ('K' : rest) = "(" ++ unparse combinatorK ++ ")" ++ preprocess rest
preprocess ('t':'r':'u':'e':rest) = preprocess $ 'K' : rest
preprocess ('c':'Z':'e':'r':'o':rest) = "(" ++ unparse zeroChurch ++ ")" ++ preprocess rest
preprocess ('b':'Z':'e':'r':'o':rest) = "(" ++ unparse zeroBarend ++ ")" ++ preprocess rest
preprocess ('c':'S':'u':'c':'c':rest) = "(" ++ unparse succChurch ++ ")" ++ preprocess rest
preprocess ('b':'S':'u':'c':'c':rest) = "(" ++ unparse succBarend ++ ")" ++ preprocess rest
preprocess ('c':'P':'r':'e':'v':rest) = "(" ++ unparse prevChurch ++ ")" ++ preprocess rest
preprocess ('b':'P':'r':'e':'v':rest) = "(" ++ unparse prevBarend ++ ")" ++ preprocess rest
preprocess ('S' : rest) = "(" ++ unparse combinatorS ++ ")" ++ preprocess rest
preprocess ('Y' : rest) = "(" ++ unparse combinatorY ++ ")" ++ preprocess rest
preprocess ('O' : rest) = "(" ++ unparse combinatorOmega ++ ")" ++ preprocess rest
preprocess (char:'_':rest)
  | char == 'c' = "(" ++ unparse (church num) ++ ")" ++ preprocess (drop (length strNum) rest)
  | char == 'b' = "(" ++ unparse (barend num) ++ ")" ++ preprocess (drop (length strNum) rest)
  | otherwise = preprocess rest
  where
    findNumber :: String -> String
    findNumber [] = []
    findNumber (char':str)
      | isDigit char' = char' : findNumber str
      | otherwise = []
    strNum :: String
    strNum = findNumber rest
    num :: Int
    num = read $ '0' : strNum
preprocess ('i':'f':' ':rest) = '(' : preprocess rest
preprocess (' ':'t':'h':'e':'n':' ':rest) = ')' : preprocess rest
preprocess (' ':'e':'l':'s':'e':rest) = preprocess rest
preprocess (' ':rest) = preprocess rest
preprocess (char:str) = char : preprocess str

parse :: String -> Maybe Lambda
parse [] = Nothing
parse ('v' : rest)
  | allPrimes rest = Just $ Var $ length rest
  where
    allPrimes :: String -> Bool
    allPrimes [] = True
    allPrimes ('\'':rest') = allPrimes rest'
    allPrimes _ = False
parse [var]
  | var `elem` varSet = Just $ Var (fromJust $ elemIndex var varSet)
  | otherwise = Nothing
parse ('\\' : var : rest)
  | null rest = Nothing
  | var == 'v' =
    let n = prefixLength rest (repeat '\'')
        tempstr = drop n rest
        rest' = if null tempstr then [] else case head tempstr of
            ',' -> '\\' : tail tempstr
            '.' -> tail tempstr
            _ -> tempstr
     in raise Abst (Just n) (parse rest')
  | head rest == ',' = raise Abst (elemIndex var varSet) (parse $ '\\' : tail rest)
  | head rest == '.' = raise Abst (elemIndex var varSet) (parse $ tail rest)
  | otherwise = Nothing
parse str
  | length objects > 1 = raise Appl (parse $ join (init objects)) (parse $ last objects)
  | head object == '(' = parse (init . tail $ object)
  where
    objects = getCombinedTerms str
    object = head objects
parse ('[':rest) = raise pair (parse s1) (parse s2)
  where
    (s1, rest') = splitAt (findSemicolon rest) rest--(take semicolonN rest, drop semicolonN rest)
    (s2, _) = splitAt (findClosingBracket (tail rest') 0) (tail rest')
    findSemicolon :: String -> Int
    findSemicolon [] = 0
    findSemicolon (';':_) = 0
    findSemicolon (_:rest'') = 1 + findSemicolon rest''
    findClosingBracket :: String -> Int -> Int
    findClosingBracket [] _ = 0
    findClosingBracket ('[' : str) step = 1 + findClosingBracket str (step + 1)
    findClosingBracket (']' : str) step
      | step == 0 = 0
      | otherwise = 1 + findClosingBracket str (step - 1)
    findClosingBracket (_ : str) step = 1 + findClosingBracket str step
parse _ = Nothing

parseJust :: String -> Lambda
parseJust = fromJust . parse

wrapAbst :: Lambda -> String
wrapAbst (Abst n l) = "(" ++ unparse (Abst n l) ++ ")"
wrapAbst l = unparse l

wrapNotVar :: Lambda -> String
wrapNotVar (Var n) = unparse (Var n)
wrapNotVar l = "(" ++ unparse l ++ ")"

unparse :: Lambda -> String
unparse (Var n)
  | n < length varSet = [varSet !! n]
  | otherwise = varSetFormal !! n
unparse (Abst n (Abst m l)) = "\\" ++ unparse (Var n) ++ "," ++ (tail . unparse) (Abst m l)
unparse (Abst n l) = "\\" ++ unparse (Var n) ++ "." ++ wrapAbst l
unparse (Appl l1 l2) = wrapAbst l1 ++ wrapNotVar l2

unparse' :: Lambda -> String
unparse' = insertSpace . unparse
  where
    insertSpace :: String -> String
    insertSpace [] = []
    insertSpace ('.':rest) = '.' : ' ' : insertSpace rest
    insertSpace (char:rest) = char : insertSpace rest

unparseFormal :: Lambda -> String
unparseFormal (Var n) = varSetFormal !! n
unparseFormal (Abst n l) = "(\\" ++ unparseFormal (Var n) ++ unparseFormal l ++ ")"
unparseFormal (Appl l1 l2) = "(" ++ unparseFormal l1 ++ unparseFormal l2 ++ ")"

-- ┌───────────────────────────┐
-- │ the logic of lambda terms │
-- └───────────────────────────┘

totalVarSet :: Lambda -> [Variable]
totalVarSet (Var n) = [n]
totalVarSet (Abst n l) = add n $ totalVarSet l
totalVarSet (Appl l1 l2) = totalVarSet l1 `union` totalVarSet l2

boundVarSet :: Lambda -> [Variable]
boundVarSet (Var _) = []
boundVarSet (Abst n l) = add n $ boundVarSet l
boundVarSet (Appl l1 l2) = boundVarSet l1 `union` boundVarSet l2

freeVarSet :: Lambda -> [Variable]
freeVarSet (Var n) = [n]
freeVarSet (Abst n l) = delete n $ freeVarSet l
freeVarSet (Appl l1 l2) = freeVarSet l1 `union` freeVarSet l2

isValid :: Lambda -> Bool
isValid (Var _) = True
isValid (Abst n l) = n `notElem` boundVarSet l && isValid l
isValid (Appl l1 l2) =
  isValid l1 && isValid l2
    && null (fv1 `intersection` bv2)
    && null (fv2 `intersection` bv1)
  where
    fv1 = freeVarSet l1
    bv1 = boundVarSet l1
    fv2 = freeVarSet l2
    bv2 = boundVarSet l2

parse' :: String -> Maybe Lambda
parse' str = adjustBoundVars <$> parse (preprocess str)

parseJust' :: String -> Lambda
parseJust' = fromJust . parse'

-- ┌───────────────────────┐
-- │ Lambda transformation │
-- └───────────────────────┘

substitute :: Lambda -> Variable -> Lambda -> Lambda
substitute (Var n) m l
  | n == m = l
  | otherwise = Var n
substitute (Abst n l1) m l2
  | n == m = Abst n l1
  | n `notElem` bnd2 = Abst n (substitute l1 m l2)
  | otherwise = Abst n (substitute l1 m l2')
  where
    bnd2 = boundVarSet l2
    l2' = changeBoundVar l2 n (1 + maximum bnd2)
substitute (Appl l1 l2) m l = Appl (substitute l1 m l) (substitute l2 m l)

changeBoundVar :: Lambda -> Variable -> Variable -> Lambda
changeBoundVar (Var n) _ _ = Var n
changeBoundVar (Abst n l) m1 m2
  | n == m1 = Abst m2 $ substitute l m1 (Var m2)
  | otherwise = Abst n $ changeBoundVar l m1 m2
changeBoundVar (Appl l1 l2) m1 m2 = Appl (changeBoundVar l1 m1 m2) (changeBoundVar l2 m1 m2)

moveBoundVars :: Lambda -> [Variable] -> Lambda
moveBoundVars l lst = foldl (uncurry' changeBoundVar) l pairlst
  where
    bv = boundVarSet l
    lst' = intersection lst bv
    vmax = 1 + max (maximum' lst) (maximum' bv)
    pairlst = zip lst' [vmax .. (vmax - 1 + length lst')]

adjustBoundVars :: Lambda -> Lambda
adjustBoundVars (Var n) = Var n
adjustBoundVars (Abst n l) = Abst n $ adjustBoundVars $ moveBoundVars l [n]
adjustBoundVars (Appl l1 l2) = Appl (adjustBoundVars l1') (adjustBoundVars l2')
  where
    fv = freeVarSet l1 `union` freeVarSet l2
    l1' = moveBoundVars l1 fv
    l2' = moveBoundVars l2 fv

reduceStep :: Lambda -> (Lambda, Bool)
reduceStep (Appl (Abst n l1) l2) = (substitute l1 n l2, True)
reduceStep (Abst n (Appl l (Var m)))
  | n == m && m `notElem` freeVarSet l = (l, True)
reduceStep (Var n) = (Var n, False)
reduceStep (Appl l1 l2)
  | found1 = (Appl l1' l2, True)
  | found2 = (Appl l1 l2', True)
  | otherwise = (Appl l1 l2, False)
  where
    (l1', found1) = reduceStep l1
    (l2', found2) = reduceStep l2
reduceStep (Abst n l) = (Abst n l', found)
  where
    (l', found) = reduceStep l

reduceLimit :: Int
reduceLimit = 1000

reduceWithLimit :: Int -> Lambda -> Lambda
reduceWithLimit n l
  | n > reduceLimit = l
  | found = reduceWithLimit (n+1) l'
  | otherwise = l
  where
    (l', found) = reduceStep l

reduce :: Lambda -> Lambda
reduce = reduceWithLimit 0

reduce' :: Lambda -> Lambda
reduce' l
  | found = reduce' l'
  | otherwise = l
  where
    (l', found) = reduceStep l

reduceTimes :: Int -> Lambda -> Lambda
reduceTimes 0 l = l
reduceTimes n l
  | found = reduceTimes (n - 1) l'
  | otherwise = l
  where
    (l', found) = reduceStep l

-- ┌─────────────┐
-- │ Labda query │
-- └─────────────┘

equiv :: Lambda -> Lambda -> Bool
equiv (Var n1) (Var n2)
  | n1 == n2 = True
  | otherwise = False
equiv (Appl p1 p2) (Appl q1 q2) = equiv p1 q1 && equiv p2 q2
equiv (Abst n1 l1) (Abst n2 l2)
  | n1 == n2 = equiv l1 l2
  | otherwise = equiv l1 (substitute l2 n2 (Var n1))
equiv _ _ = False

equiv' :: Lambda -> Lambda -> Bool
equiv' l1 l2 = equiv (reduce l1) (reduce l2)
