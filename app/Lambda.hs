module Lambda where

-- import Control.Monad
import Data.Char (isAlpha, isDigit, isSpace, isNumber)
import Data.List (elemIndex, intercalate)
import Data.Maybe (fromJust, isJust)
import Text.Read (readMaybe)
import Data.Set (Set, empty, insert, delete, union, intersection, difference, singleton, findMax, toList, member)

-- ┌───────────────────────────┐
-- │ the lambda calculus model │
-- └───────────────────────────┘

-- varSetFormal :: [String]
-- varSetFormal = ['v' : replicate n '\'' | n <- [0 ..]]

varSet :: [Char]
varSet = ['x', 'y', 'z', 'w', 'u', 't', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'p', 'q', 'n', 'm', 'k']

type Variable = Int
data Lambda = Var Variable | Abst Variable Lambda | Appl Lambda Lambda deriving (Read, Show, Eq)

-- Basic combinators

combinatorI :: Lambda
combinatorI = Abst 0 (Var 0)

combinatorK :: Lambda
combinatorK = Abst 0 (Abst 1 (Var 0))
true :: Lambda
true = combinatorK

combinatorK' :: Lambda
combinatorK' = Abst 0 (Abst 1 (Var 1))
false :: Lambda
false = combinatorK'

combinatorS :: Lambda -- \x,y,z. xy(xz)
combinatorS = Abst 0 (Abst 1 (Abst 2 (Appl (Appl (Var 0) (Var 2)) (Appl (Var 1) (Var 2)))))

combinatorY :: Lambda -- \f. (\x.f(xx)) (\x.f(xx))
combinatorY = Abst 1 (Appl (Abst 0 (Appl (Var 1) (Appl (Var 0) (Var 0)))) (Abst 0 (Appl (Var 1) (Appl (Var 0) (Var 0)))))

combinatorOmega :: Lambda -- (\x. xx)(\x. xx)
combinatorOmega = Appl (Abst 0 $ Appl (Var 0) (Var 0)) (Abst 0 $ Appl (Var 0) (Var 0))

combinatorNeg :: Lambda
combinatorNeg = Abst 0 (Appl (Appl (Var 0) (Abst 4 (Abst 3 (Var 3)))) (Abst 4 (Abst 3 (Var 4))))

power :: Int -> Lambda -> Lambda
power 1 l = l
power 0 _ = combinatorI
power n l = Appl (power (n-1) l) l

omegaSmall :: Int -> Lambda
omegaSmall n = Abst 0 (power n (Var 0))

omegaBig :: Int -> Lambda
omegaBig n = let on = omegaSmall n in Appl on on

-- Church numbers

church :: Int -> Lambda
church 0 = adjustBoundVars $ Abst 0 (Abst 1 (Var 1))
church n = Abst 0 (Abst 1 $ Appl (Var 0) (reduce $ Appl (Appl (church (n-1)) (Var 0)) (Var 1)))

zeroChurch :: Lambda
zeroChurch = Abst 0 (Appl (Appl (Var 0) (Appl (Abst 1 (Abst 2 (Var 1))) (Abst 1 (Abst 2 (Var 2))))) (Abst 1 (Abst 2 (Var 1))))

succChurch :: Lambda
succChurch = Abst 2 (Abst 0 (Abst 1 (Appl (Appl (Var 2) (Var 0)) (Appl (Var 0) (Var 1)))))

prevChurch :: Lambda
prevChurch = Abst 0 (Abst 1 (Abst 2 (Appl (Appl (Appl (Var 0) (Abst 14 (Abst 15 (Appl (Var 15) (Appl (Var 14) (Var 1)))))) (Abst 3 (Var 2))) (Abst 16 (Var 16)))))

addChurch :: Lambda
addChurch = Abst 0 (Abst 1 (Abst 14 (Abst 15 (Appl (Appl (Var 0) (Var 14)) (Appl (Appl (Var 1) (Var 14)) (Var 15))))))

multChurch :: Lambda
multChurch = Abst 0 (Abst 1 (Abst 2 (Appl (Var 0) (Appl (Var 1) (Var 2)))))

expChurch :: Lambda
expChurch = Abst 0 (Abst 1 (Appl (Var 1) (Var 0)))

pair :: Lambda -> Lambda -> Lambda
pair l1 l2 = Abst n (Appl (Appl (Var n) l1) l2)
  where n = 1 + max (findMax $ totalVarSet l1) (findMax $ totalVarSet l2)

-- Barendregt numbers

barend :: Int -> Lambda
barend 0 = combinatorI
barend n = pair false (barend $ n - 1)

zeroBarend :: Lambda
zeroBarend = Abst 0 (Appl (Var 0) (Abst 2 (Abst 1 (Var 2))))

succBarend :: Lambda
succBarend = Abst 11 (Abst 0 (Appl (Appl (Var 0) (Abst 2 (Abst 1 (Var 1)))) (Var 11)))

prevBarend :: Lambda
prevBarend = Abst 11 (Appl (Var 11) (Abst 0 (Abst 1 (Var 1))))

-- ┌──────────────────────┐
-- │ parsing lambda terms │
-- └──────────────────────┘

preprocess :: String -> String
preprocess [] = []
preprocess (',':rest) = '.' : '\\' : preprocess rest
----------
preprocess ('c':':':'Z':'e':'r':'o':rest) = "#cz#" ++ preprocess rest
preprocess ('c':':':'S':'+':rest) = "#cs#" ++ preprocess rest
preprocess ('c':':':'P':'-':rest) = "#cp#" ++ preprocess rest
preprocess ('c':':':'A':'d':'d':rest) = "#ca#" ++ preprocess rest
preprocess ('c':':':'M':'u':'l':'t':rest) = "#cm#" ++ preprocess rest
preprocess ('c':':':'E':'x':'p':rest) = "#ce#" ++ preprocess rest
----------
preprocess ('b':':':'Z':'e':'r':'o':rest) = "#bz#" ++ preprocess rest
preprocess ('b':':':'S':'+':rest) = "#bs#" ++ preprocess rest
preprocess ('b':':':'P':'-':rest) = "#bp#" ++ preprocess rest
preprocess (c:':':rest) = let (num, rest') = takeWhile' False isNumber rest in "#" ++ [c] ++ ":" ++ num ++ "#" ++ preprocess rest'
----------
preprocess ('f':'a':'l':'s':'e':rest) = "#F#" ++ preprocess rest
preprocess ('t':'r':'u':'e':rest) = "#T#" ++ preprocess rest
preprocess ('i':'f':' ':rest) = '(' : preprocess rest
preprocess (' ':'t':'h':'e':'n':' ':rest) = ')' : preprocess rest
preprocess (' ':'e':'l':'s':'e':rest) = preprocess rest
----------
preprocess ('I' : rest) = "#I#" ++ preprocess rest
preprocess ('K' : '*' : rest) = "#F#" ++ preprocess rest
preprocess ('K' : rest) = "#T#" ++ preprocess rest
preprocess ('F' : rest) = "#F#" ++ preprocess rest
preprocess ('T' : rest) = "#T#" ++ preprocess rest
preprocess ('S' : rest) = "#S#" ++ preprocess rest
preprocess ('Y' : rest) = "#Y#" ++ preprocess rest
preprocess ('O' : rest) = "#O#" ++ preprocess rest
preprocess ('N' : rest) = "#N#" ++ preprocess rest
----------
preprocess (char:str) = char : preprocess str

data Output a = Content a | Error String String deriving (Show, Read, Eq)
instance Functor Output where
  fmap f oa = case oa of
    Error str trace -> Error str trace
    Content a -> Content (f a)
instance Applicative Output where
  pure = Content
  mf <*> ma = case mf of
    Error str trace -> Error str trace
    Content f -> fmap f ma
instance Monad Output where
  return = pure
  mval >>= f = case mval of
    Error str trace -> Error str trace
    Content a -> f a

wrap :: String -> String -> Maybe a -> Output a
wrap _ _ (Just x) = Content x
wrap msg trace Nothing = Error msg trace

data Token = VarToken Int | NamedExpr String | BackSlash | Dot | Grouped [Token] deriving (Show, Read, Eq)

(<:>) :: (Monad m) => a -> m [a] -> m [a]
(<:>) x = fmap (x :)
infixr 5 <:>

takeWhile' :: Bool -> (a -> Bool) -> [a] -> ([a], [a])
takeWhile' _ _ [] = ([], [])
takeWhile' drp test (x : rest)
  | test x = let (l, r) = takeWhile' drp test rest in (x : l, r)
  | otherwise = ([], if drp then rest else x : rest)

findMatching :: Eq a => Int -> a -> a -> [a] -> Output ([a], [a])
findMatching _ _ _ [] = Error "Unbalanced parentheses" []
findMatching n op cl (c : rest)
  | c == cl = if n == 0 then Content ([], rest) else do
      (l, r) <- findMatching (n-1) op cl rest
      return (c : l, r)
  | c == op = do
      (l, r) <- findMatching (n+1) op cl rest
      return (c : l, r)
  | otherwise = do
      (l, r) <- findMatching n op cl rest
      return (c : l, r)

tokenize :: String -> Output [Token]
tokenize [] = Content []
tokenize (c : rest)
  | isSpace c = tokenize rest
tokenize ('\\' : rest) = BackSlash <:> tokenize rest
tokenize ('.' : rest) = Dot <:> tokenize rest
tokenize ('(' : rest) = do -- )
  (str, rest') <- findMatching 0 '(' ')' rest
  grouped <- Grouped <$> tokenize str
  grouped <:> tokenize rest'
tokenize ('v' : rest) = case takeWhile' False isNumber rest of
  ([], _) -> Error "The `v` character must be followed by a number" []
  (num, rest') ->
    wrap "Could not read number" num (readMaybe num)
    >>= \n -> VarToken n <:> tokenize rest'
tokenize ('#' : rest) = let (str, rest') = takeWhile' True (/= '#') rest in NamedExpr str <:> tokenize rest'
tokenize (c : rest) =
  (wrap "cannot find in the variable set" [c] (elemIndex c varSet))
  >>= \n -> VarToken n <:> tokenize rest

parse :: [Token] -> Output Lambda
parse [VarToken n] = Content (Var n)
parse [NamedExpr str] = case str of
  "cz" -> Content zeroChurch
  "cs" -> Content succChurch
  "cp" -> Content prevChurch
  "ca" -> Content addChurch
  "cm" -> Content multChurch
  "ce" -> Content expChurch
  "bz" -> Content zeroBarend
  "bs" -> Content succBarend
  "bp" -> Content prevBarend
  (c : ':' : num) -> wrap "failed to parse number" num (readMaybe num) >>= \n -> case c of
    'c' -> Content (church n)
    'b' -> Content (barend n)
    'o' -> Content (omegaSmall n)
    'O' -> Content (omegaBig n)
    _ -> Error "unrecornized combinator" [c]
  ['I'] -> Content combinatorI
  ['F'] -> Content combinatorK'
  ['T'] -> Content combinatorK
  ['S'] -> Content combinatorS
  ['Y'] -> Content combinatorY
  ['O'] -> Content combinatorOmega
  ['N'] -> Content combinatorNeg
  _ -> Error "unregognized named expression" str
parse [Grouped tks] = parse tks
parse [t1, t2] = liftA2 Appl (parse [t1]) (parse [t2])
parse (BackSlash : VarToken n : Dot : rest) = fmap (Abst n) (parse rest)
parse (t1 : t2 : rest) = parse (Grouped [t1, t2] : rest)
parse [] = Error "cannot parse an empty expression" []
parse (t : _) = Error "invalid syntax at token" (show t)

fullParse :: String -> Output Lambda
fullParse str = tokenize (preprocess str) >>= \tks -> parse tks

-- wrapAbst :: (Lambda -> String) -> Lambda -> String
-- wrapAbst f l = case f l of
--   [a] -> [a]
--   str -> case l of
--     Abst _ _ -> "(" ++ str ++ ")"
--     _ -> str

wrapNotSingle :: String -> String
wrapNotSingle [a] = [a]
wrapNotSingle str = "(" ++ str ++ ")"

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

getVarName :: Bool -> Variable -> String
getVarName sugar n
  | sugar && n < length varSet = [varSet !! n]
  | otherwise = 'v' : show n

unparse :: Bool -> Bool -> Lambda -> String
unparse _ True l
  | congr l combinatorI = "I"
  | congr l combinatorK = "K"
  | congr l combinatorK' = "K*"
  | congr l combinatorS = "S"
  | congr l combinatorY = "Y"
  | congr l combinatorOmega = "O"
  | isJust theChurch = "c:" ++ (show . fromJust $ theChurch)
  | isJust theBarend = "b:" ++ (show . fromJust $ theBarend)
    where
      theChurch = recognizeChurch l
      theBarend = recognizeBarend l
unparse sugar _ (Var n) = getVarName sugar n
unparse sugar alias (Abst n (Abst m l))
  | sugar = "\\" ++ unparse True alias (Var n) ++ case unparse True alias (Abst m l) of
      '\\' : str -> ',' : str
      str -> '.' : str
  | otherwise = "\\" ++ unparse False alias (Var n) ++ "." ++ unparse False alias (Abst m l)
unparse sugar alias (Abst n l) =
  "\\" ++ unparse sugar alias (Var n) ++ (if sugar then ". " else ".") ++ unparse sugar alias l
unparse sugar alias (Appl (Abst v l1) l2) = "(" ++ unparse sugar alias (Abst v l1) ++ ")" ++ wrapNotSingle (unparse sugar alias l2)
unparse sugar alias (Appl l1 l2) = unparse sugar alias l1 ++ wrapNotSingle (unparse sugar alias l2)

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

printVarSet :: Bool -> Set Variable -> String
printVarSet sugar vars = "{" ++ (intercalate ", " . map (getVarName sugar) . toList) vars ++ "}"

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

-- ┌────────────────────────────┐
-- │ lambda term transformation │
-- └────────────────────────────┘

substitute :: Lambda -> Variable -> Lambda -> Lambda
substitute src var expr
  | var `member` boundVarSet src = src
  | otherwise = substitute'
      (moveBoundVars src (freeVarSet expr))
      var
      (moveBoundVars expr (difference (totalVarSet src) (singleton var)))
  where
    substitute' :: Lambda -> Variable -> Lambda -> Lambda
    substitute' (Var n) m expr'
      | n == m = expr'
      | otherwise = Var n
    substitute' (Abst n src') m expr' = Abst n (substitute' src' m expr')
    substitute' (Appl src1' src2') m expr' = Appl (substitute' src1' m expr') (substitute' src2' m expr')

substituteVar :: Lambda -> Variable -> Variable -> Lambda
substituteVar (Var n) from to
  | n == from = Var to
  | otherwise = Var n
substituteVar (Abst n l) from to = Abst n $ substituteVar l from to
substituteVar (Appl l1 l2) from to = Appl (substituteVar l1 from to) (substituteVar l2 from to)

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

reduceWithLimit :: Int -> Lambda -> Lambda
reduceWithLimit 0 l = l
reduceWithLimit lim l
  | found = reduceWithLimit (lim - 1) l'
  | otherwise = l
  where
    (l', found) = reduceStep l

reduce :: Lambda -> Lambda
reduce = reduceWithLimit reduceLimit

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

-- ┌──────────────┐
-- │ lambda query │
-- └──────────────┘

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
