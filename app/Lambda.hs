module Lambda where

import Control.Monad
import Data.Char (isAlpha, isDigit)
import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust)
import Text.Read (readMaybe)
import Data.Set (Set, empty, insert, delete, union, intersection, singleton, findMax, toList, member)

getCombinedTerms :: String -> [String]
getCombinedTerms [] = []
getCombinedTerms (var : rest)
  | null rest = [[var]]
  | isAlpha var = (var : take n1 rest) : getCombinedTerms (drop n1 rest)
  | var == '(' = (var : take n2 rest) : getCombinedTerms (drop n2 rest) -- )
  | var == '[' = (var : take n3 rest) : getCombinedTerms (drop n3 rest) -- ]
  | otherwise = [var] : getCombinedTerms rest
  where
    findAlpha :: String -> Int
    findAlpha [] = 0
    findAlpha (a : as)
      | a == '(' = 0 -- )
      | a == '[' = 0 -- ]
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
combinatorK = parseJust "\\x.\\y.x"
true :: Lambda
true = combinatorK

combinatorK' :: Lambda
combinatorK' = parseJust "\\x.\\y.y"
false :: Lambda
false = combinatorK'

combinatorS :: Lambda
combinatorS = parseJust "\\x.\\y.\\z.xz(yz)"

combinatorY :: Lambda
combinatorY = parseJust "\\f.(\\x.f(xx))(\\x.f(xx))"

combinatorOmega :: Lambda
combinatorOmega = parseJust "(\\x.xx)(\\x.xx)"

combinatorNeg :: Lambda
combinatorNeg = parseJust "\\x.x(\\u.\\w.w)(\\u.\\w.u)"

omegaSmall :: Int -> Lambda
omegaSmall n = parseJust $ "\\x." ++ replicate n 'x'

omegaBig :: Int -> Lambda
omegaBig n = Appl (omegaSmall n) (omegaSmall n)

church :: Int -> Lambda
church 0 = adjustBoundVars $ Abst 0 (Abst 1 (Var 1))
church n = Abst 0 (Abst 1 $ Appl (Var 0) (reduce $ Appl (Appl (church (n-1)) (Var 0)) (Var 1)))
zeroChurch :: Lambda
zeroChurch = parseJust "\\x.x((\\y.\\z.y)(\\y.\\z.z))(\\y.\\z.y)"
succChurch :: Lambda
succChurch = parseJust "\\f.\\x.\\y.fx(xy)"
prevChurch :: Lambda
prevChurch = parseJust "\\x.\\y.\\z.x(\\p.\\q.q(py))((\\u.\\w.u)z)(\\t.t)"
addChurch :: Lambda
addChurch = parseJust "\\x.\\y.\\p.\\q.xp(ypq)"
multChurch :: Lambda
multChurch = parseJust "\\x.\\y.\\z.x(yz)"
expChurch :: Lambda
expChurch = parseJust "\\x.\\y.yx"

pair :: Lambda -> Lambda -> Lambda
pair l1 l2 = Abst n (Appl (Appl (Var n) l1) l2)
  where
    n = 1 + max (findMax $ totalVarSet l1) (findMax $ totalVarSet l2)

barend :: Int -> Lambda
barend 0 = combinatorI
barend n = pair false (barend $ n - 1)
zeroBarend :: Lambda
zeroBarend = parseJust "\\x.x(\\y.\\z.y)"
succBarend :: Lambda
succBarend = parseJust "\\f.\\x.x(\\y.\\z.z)f"
prevBarend :: Lambda
prevBarend = parseJust "\\f.f(\\x.\\y.y)"

-- ┌──────────────────────┐
-- │ Parsing lambda terms │
-- └──────────────────────┘

preprocess :: String -> String
preprocess [] = []
preprocess (',':rest) = '.' : '\\' : preprocess rest
----------
preprocess ('c':':':'Z':'e':'r':'o':rest) = "(" ++ unparse False zeroChurch ++ ")" ++ preprocess rest
preprocess ('c':':':'S':'+':rest) = "(" ++ unparse False succChurch ++ ")" ++ preprocess rest
preprocess ('c':':':'P':'-':rest) = "(" ++ unparse False prevChurch ++ ")" ++ preprocess rest
preprocess ('c':':':'A':'d':'d':rest) = "(" ++ unparse False addChurch ++ ")" ++ preprocess rest
preprocess ('c':':':'M':'u':'l':'t':rest) = "(" ++ unparse False multChurch ++ ")" ++ preprocess rest
preprocess ('c':':':'E':'x':'p':rest) = "(" ++ unparse False expChurch ++ ")" ++ preprocess rest
----------
preprocess ('b':':':'Z':'e':'r':'o':rest) = "(" ++ unparse False zeroBarend ++ ")" ++ preprocess rest
preprocess ('b':':':'S':'+':rest) = "(" ++ unparse False succBarend ++ ")" ++ preprocess rest
preprocess ('b':':':'P':'-':rest) = "(" ++ unparse False prevBarend ++ ")" ++ preprocess rest
preprocess (char:':':rest)
  | char == 'c' = "(" ++ unparse False (church num) ++ ")" ++ preprocess (drop (length strNum) rest)
  | char == 'b' = "(" ++ unparse False (barend num) ++ ")" ++ preprocess (drop (length strNum) rest)
  | char == 'o' = "(" ++ unparse False (omegaSmall num) ++ ")" ++ preprocess (drop (length strNum) rest)
  | char == 'O' = "(" ++ unparse False (omegaBig num) ++ ")" ++ preprocess (drop (length strNum) rest)
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
----------
preprocess ('f':'a':'l':'s':'e':rest) = preprocess $ 'K' : '*' : rest
preprocess ('t':'r':'u':'e':rest) = preprocess $ 'K' : rest
preprocess ('i':'f':' ':rest) = '(' : preprocess rest
preprocess (' ':'t':'h':'e':'n':' ':rest) = ')' : preprocess rest
preprocess (' ':'e':'l':'s':'e':rest) = preprocess rest
----------
preprocess ('I' : rest) = "(" ++ unparse False combinatorI ++ ")" ++ preprocess rest
preprocess ('K' : '*' : rest) = "(" ++ unparse False combinatorK' ++ ")" ++ preprocess rest
preprocess ('K' : rest) = "(" ++ unparse False combinatorK ++ ")" ++ preprocess rest
preprocess ('S' : rest) = "(" ++ unparse False combinatorS ++ ")" ++ preprocess rest
preprocess ('Y' : rest) = "(" ++ unparse False combinatorY ++ ")" ++ preprocess rest
preprocess ('O' : rest) = "(" ++ unparse False combinatorOmega ++ ")" ++ preprocess rest
preprocess ('N' : rest) = "(" ++ unparse False combinatorNeg ++ ")" ++ preprocess rest
----------
preprocess (' ':rest) = preprocess rest
preprocess (char:str) = char : preprocess str

parse :: String -> Maybe Lambda
parse [] = Nothing
parse ('v' : rest)
  | isJust num =  fmap Var num
  | allPrimes rest = Just $ Var $ length rest
  where
    num :: Maybe Int
    num = readMaybe rest
    allPrimes :: String -> Bool
    allPrimes [] = True
    allPrimes ('\'':rest') = allPrimes rest'
    allPrimes _ = False
parse [var]
  | var `elem` varSet = Just $ Var (fromJust $ elemIndex var varSet)
  | otherwise = Nothing
parse ('\\' : 'v' : char : rest)
  | isDigit char =
    let f :: String -> String -> (Maybe String, Maybe String)
        f _ [] = (Nothing, Nothing)
        f acc ('.':rest'') = (Just acc, Just rest'')
        f acc (char':rest'') = f (acc ++ [char']) rest''
        (numStr, rest') = f [] (char:rest)
        var = numStr >>= readMaybe
     in raise Abst var (rest' >>= parse')
  | char == '\'' = 
    let f :: Int -> String -> Maybe Lambda
        f n ('\'' : rest') = f (n+1) rest'
        f n ('.' : rest') = raise Abst (Just n) (parse' rest')
        f _ _ = Nothing
     in f 1 rest
parse ('\\' : var : '.' : rest) = raise Abst (elemIndex var varSet) (parse rest)
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

wrapAbst :: (Lambda -> String) -> Lambda -> String
wrapAbst f (Abst n l) = "(" ++ f (Abst n l) ++ ")"
wrapAbst f l = f l

wrapNotVar :: (Lambda -> String) -> Lambda -> String
wrapNotVar f (Var n) = f (Var n)
wrapNotVar f l = "(" ++ f l ++ ")"

recognizeChurch :: Lambda -> Maybe Int
recognizeChurch (Abst n1 (Abst n2 l)) = f l
  where
    f :: Lambda -> Maybe Int
    f (Var n)
      | n == n2 = Just 0
    f (Appl (Var n1') l')
      | n1' == n1 = (1+) <$> f l'
    f _ = Nothing
recognizeChurch _ = Nothing

recognizeBarend :: Lambda -> Maybe Int
recognizeBarend l
  | congr l (Abst 0 (Var 0)) = Just 0
  | otherwise = case l of
    Abst n1 (Appl (Appl (Var n2) l1) l2) ->
      if n1 == n2 && congr l1 combinatorK'
      then (1+) <$> recognizeBarend l2
      else Nothing
    _ -> Nothing

unparse :: Bool -> Lambda -> String
unparse True l
  | congr l combinatorI = "I"
  | congr l combinatorK = "K"
  | congr l combinatorK' = "K*"
  | congr l combinatorS = "S"
  | congr l combinatorY = "Y"
  | congr l combinatorOmega = "O" 
  | isChurch = "c:" ++ (show . fromJust $ theChurch)
  | isBarend = "b:" ++ (show . fromJust $ theBarend)
    where
      theChurch = recognizeChurch l
      isChurch = case theChurch of
        Nothing -> False
        Just _ -> True
      theBarend = recognizeBarend l
      isBarend = case theBarend of
        Nothing -> False
        Just _ -> True
unparse sugar (Var n)
  | n < length varSet = [varSet !! n]
  | sugar = 'v' : show n
  | otherwise = varSetFormal !! n
unparse sugar (Abst n (Abst m l))
  | sugar = "\\" ++ unparse True (Var n) ++ unparsedRest
  | otherwise = "\\" ++ unparse False (Var n) ++ "." ++ unparse False (Abst m l)
    where
      unparsedRest = case unparse True (Abst m l) of
        '\\' : str -> ',' : str
        str -> '.' : str
unparse sugar (Abst n l) =
  "\\" ++ unparse sugar  (Var n) ++ (if sugar then ". " else ".") ++ wrapAbst (unparse sugar) l
unparse sugar (Appl l1 l2) = wrapAbst (unparse sugar) l1 ++ wrapNotVar (unparse sugar) l2

unparseFormal :: Lambda -> String
unparseFormal (Var n) = varSetFormal !! n
unparseFormal (Abst n l) = "(\\" ++ unparseFormal (Var n) ++ unparseFormal l ++ ")"
unparseFormal (Appl l1 l2) = "(" ++ unparseFormal l1 ++ unparseFormal l2 ++ ")"

-- ┌───────────────────────────┐
-- │ the logic of lambda terms │
-- └───────────────────────────┘

totalVarSet :: Lambda -> Set Variable
totalVarSet (Var n) = singleton n
totalVarSet (Abst n l) = insert n $ totalVarSet l
totalVarSet (Appl l1 l2) = totalVarSet l1 `union` totalVarSet l2

boundVarSet :: Lambda -> Set Variable
boundVarSet (Var _) = empty
boundVarSet (Abst n l) = insert n $ boundVarSet l
boundVarSet (Appl l1 l2) = boundVarSet l1 `union` boundVarSet l2

freeVarSet :: Lambda -> Set Variable
freeVarSet (Var n) = singleton n
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
substitute (Var n) m expr
  | n == m = expr
  | otherwise = Var n
substitute (Abst n src) m expr
  | n == m = Abst n src
  | n `member` exprtvs = Abst n' (substitute (substituteVar src n n') m expr)
  | otherwise = Abst n (substitute src m expr)
    where
      exprtvs = totalVarSet expr
      n' = 1 + max (findMax $ totalVarSet src) (findMax exprtvs)
substitute (Appl src1 src2) m expr = Appl (substitute src1 m expr) (substitute src2 m expr)

substituteVar :: Lambda -> Variable -> Variable -> Lambda
substituteVar (Var n') from to
  | n' == from = Var to
  | otherwise = Var n'
substituteVar (Abst n' l') from to = Abst n' $ substituteVar l' from to
substituteVar (Appl l1' l2') from to = Appl (substituteVar l1' from to) (substituteVar l2' from to)

changeBoundVar :: Lambda -> Variable -> Variable -> Lambda
changeBoundVar (Var n) _ _ = Var n
changeBoundVar (Abst n l) m1 m2
  | n == m1 = Abst m2 $ substituteVar l m1 m2
  | otherwise = Abst n $ changeBoundVar l m1 m2
changeBoundVar (Appl l1 l2) m1 m2 = Appl (changeBoundVar l1 m1 m2) (changeBoundVar l2 m1 m2)

moveBoundVars :: Lambda -> Set Variable -> Lambda
moveBoundVars l lst = foldl (uncurry' changeBoundVar) l pairlst
  where
    bv = boundVarSet l
    lst' = intersection lst bv
    vmax = 1 + max (findMax lst) (findMax bv)
    pairlst = zip (toList lst') [vmax .. (vmax - 1 + length lst')]
    uncurry' :: (a -> b -> c -> d) -> a -> (b, c) -> d
    uncurry' f a = uncurry (f a)

adjustBoundVars :: Lambda -> Lambda
adjustBoundVars (Var n) = Var n
adjustBoundVars (Abst n l) = Abst n $ adjustBoundVars $ moveBoundVars l (singleton n)
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

reduceWithLimit :: Int -> Int -> Lambda -> Lambda
reduceWithLimit n lim l
  | n >= lim = l
  | found = reduceWithLimit (n+1) lim l'
  | otherwise = l
  where
    (l', found) = reduceStep l

reduce :: Lambda -> Lambda
reduce = reduceWithLimit 0 reduceLimit

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

congr :: Lambda -> Lambda -> Bool
congr (Var n1) (Var n2)
  | n1 == n2 = True
  | otherwise = False
congr (Appl p1 p2) (Appl q1 q2) = congr p1 q1 && congr p2 q2
congr (Abst n1 l1) (Abst n2 l2)
  | n1 == n2 = congr l1 l2
  | otherwise = congr l1 (substitute l2 n2 (Var n1))
congr _ _ = False

equiv :: Lambda -> Lambda -> Bool
equiv l1 l2 = congr (reduce l1) (reduce l2)

-- inconsistent :: Lambda -> Lambda -> Maybe
