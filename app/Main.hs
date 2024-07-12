module Main where

import Lambda
import System.Console.Haskeline
import System.Environment (getArgs)
import Data.Map (Map, empty, insert, member, notMember, (!), elems, singleton)
import qualified Data.Map as M
import Text.Read (readMaybe)

-- ┌─────────────────────┐
-- │ Command definitions │
-- └─────────────────────┘

data Mode = RETURN | REPEAT
  | PRINT | EXPAND | SUBS | REDUCE | REDUCELIMIT | STEPS
  | CONGR | EQUIV
  | SHOW | READ
  | TOFORMAL | TOINFORMAL
  | LET | WITHBIND | ASSIGN
  deriving (Read, Show)

data Output a = Content a | Error String String
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

type Action a = InputT IO (Output a)
type Bindings = Map Char String

mapFromList :: (Ord a) => [(a, a, b, a)] -> Map a b
mapFromList [] = empty
mapFromList ((a1,a2,b,_):rest) =
  insert a1 b $
  insert a2 b $
  mapFromList rest

commandList :: [(String, String, Mode, String)]
commandList = [
    ("rt", "return", RETURN, "swallows input and returns it"),
    ("rp", "repeat", REPEAT, "repeats input as-is"),
    ("pr", "print", PRINT, "prints a given lambda term, applying aliases"),
    ("ex", "expand", EXPAND, "expands given aliases into the standard lambda syntax"),
    ("sb", "subs", SUBS, "substitutes a given expression in place of a given variable"),
    ("r", "reduce", REDUCE, "reduces a given lambda expression and prints it"),
    ("rl", "reducelimit", REDUCELIMIT, "gives control over the depth of reduction"),
    ("rs", "steps", STEPS, "enters a step-by-step reduction process"),
    ("cr", "congr", CONGR, "prints whether two given expressions are congruent (with variable replacement)"),
    ("eq", "equiv", EQUIV, "prints whether two given expressions are reducible to the same one"),
    ("sh", "show", SHOW, "prints the internal representation of a lambda expression"),
    ("rd", "read", READ, "evaluates a given internal representation and prints it"),
    ("tf", "toformal", TOFORMAL, "converts to the formal notation"),
    ("ti", "toinformal", TOINFORMAL, "converts to the informal notation"),
    ("lt", "let", LET, "allows to set a binding, order (binding, expression)"),
    ("wb", "withbind", WITHBIND, "allows to set a binding, order (expression, binding)"),
    ("as", "assign", ASSIGN, "a three-step action similar to LET and WITHBIND, but allows piping into bindings")
  ]

modeMap :: Map String Mode
modeMap = mapFromList commandList

-- ┌────────────────────────────────┐
-- │ Input/Output string processing │
-- └────────────────────────────────┘

(>>==) :: (Monad m) => Output a -> (a -> m (Output b)) -> m (Output b)
(>>==) (Error str trace) _ = return (Error str trace)
(>>==) (Content a) f = f a

infixr 9 <.>
(<.>) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(<.>) g f a b = g (f a b)

calibrateLengthPost :: Int -> String -> String
calibrateLengthPost n str
  | len > n = str
  | otherwise = str ++ replicate (n - len) ' '
  where
    len = length str

parseOutput' :: String -> Output Lambda
parseOutput' str = case parse' str of
  Nothing -> Error "Could not parse lambda expression" str
  Just l -> Content l

readOutput :: (Read a) => String -> Output a
readOutput str = case readMaybe str of
  Nothing -> Error "Read error" str
  Just a -> Content a

parseBinding :: String -> Output (Char, String)
parseBinding [] = Error "No input given" []
parseBinding (' ':rest) = parseBinding rest
parseBinding (char:' ':rest) = parseBinding (char:rest)
parseBinding (char:'=':' ':rest) = parseBinding (char:'=':rest)
parseBinding (char:'=':rest)
  | null rest = Error "No body given for" [char]
  | otherwise = Content (char, rest)
parseBinding ('=':_) = Error "No name given" []
parseBinding str = Error "Incorrect syntax" str

useBindings :: Bindings -> String -> String
useBindings _ [] = []
useBindings bindings (char:rest)
  | char `member` bindings = "(" ++ (bindings ! char) ++ ")" ++ useBindings bindings rest
  | otherwise = char : useBindings bindings rest

parseWithBindings' :: Bindings -> String -> Output Lambda
parseWithBindings' bindings = parseOutput' . useBindings bindings

replaceChar :: (Char, String) -> String -> String
replaceChar = uncurry $ useBindings <.> singleton

color :: String -> String -> String
color typ str = "\ESC[" ++ typ ++ "m" ++ str ++ "\ESC[0m" -- ]]

promptLength :: Int
promptLength = 18

getPrompt :: Char -> String -> String
getPrompt rep body = "\ESC[0m(" ++ color "33" body ++ ") " ++ replicate n rep ++ ": \ESC[34m" -- ]]
  where n = promptLength - length body - 5

getColoredInputLine :: String -> Action String
getColoredInputLine pref = do
  res <- getInputLine pref
  outputStr "\ESC[0m" -- ]
  case res of
    Nothing -> return (Error "Input error" [])
    Just str -> return (Content str)

-- ┌─────────────────────────────────┐
-- │ Evaluating and returning output │
-- └─────────────────────────────────┘

printGeneral :: (String -> InputT IO ()) -> Output String -> Action String
printGeneral _ (Error str trace) = return (Error str trace)
printGeneral f (Content str) = do
  f . ("| " ++) . color "34" $ str
  return $ Content str

printLn :: Output String -> Action String
printLn = printGeneral outputStrLn

withTwo ::
  (a -> b -> c) ->
  (String -> Output a) -> (String -> Output b) ->
  (c -> String) ->
  String ->
  Bindings -> String -> Action String
withTwo f readA readB showC prompt bindings input = (readA . useBindings bindings) input >>== \a -> do
  minput <- getColoredInputLine $ getPrompt ' ' prompt
  mstr2 <- minput >>== eval RETURN bindings
  let mb = mstr2 >>= (readB . useBindings bindings)
  mb >>== \b -> printLn $ Content (showC $ f a b)

withThree ::
  (a -> b -> c -> d) ->
  (String -> Output a) -> (String -> Output b) -> (String -> Output c) ->
  (d -> String) ->
  (String, String) ->
  Bindings -> String -> Action String
withThree f readA readB readC showC (prompt1, prompt2) bindings str1 =
  (readA . useBindings bindings) str1 >>== \a -> do
    minput2 <- getColoredInputLine $ getPrompt ' ' prompt1
    mstr2 <- minput2 >>== eval RETURN bindings
    let mb = mstr2 >>= (readB . useBindings bindings)
    mb >>== \b -> do
      minput3 <- getColoredInputLine $ getPrompt ' ' prompt2
      mstr3 <- minput3 >>== eval RETURN bindings
      let mc = mstr3 >>= (readC . useBindings bindings)
      mc >>== \c -> printLn $ Content (showC $ f a b c)

evalOnce :: Mode -> Bindings -> String -> Action String
evalOnce RETURN = (return . Content) <.> useBindings
evalOnce REPEAT = (printLn . Content) <.> useBindings
evalOnce PRINT = (printLn . fmap (unparse True True)) <.> parseWithBindings'
evalOnce EXPAND = (printLn . fmap (unparse True False)) <.> parseWithBindings'
evalOnce SUBS = withThree substitute parseOutput' getVar parseOutput' (unparse True False) ("VAR", "EXPR")
  where
    getVar :: String -> Output Int
    getVar str = case parseOutput' str of
      Error str' trace -> Error str' trace
      Content (Var n) -> Content n
      _ -> Error "Expression is not a variable" str
evalOnce REDUCE = (printLn . fmap (unparse True True . reduce)) <.> parseWithBindings'
evalOnce REDUCELIMIT = withTwo (flip $ reduceWithLimit 0) parseOutput' readOutput (unparse True True) "LIMIT"
evalOnce STEPS = print' <.> parseWithBindings'
  where
    print' :: Output Lambda -> Action String
    print' (Error str trace) = return (Error str trace)
    print' (Content l) = showSteps l
    showSteps :: Lambda -> Action String
    showSteps l = do
      let lstr = unparse True False l
      _ <- printGeneral outputStr (Content lstr)
      input <- getColoredInputLine ""
      case input of
        Error _ _ -> return (Content lstr)
        _ -> do
          let (l', found) = reduceStep l
          if found
          then showSteps l'
          else return $ Content lstr
evalOnce CONGR = withTwo congr parseOutput' parseOutput' show "AND"
evalOnce EQUIV = withTwo equiv parseOutput' parseOutput' show "AND"
evalOnce SHOW = (printLn . fmap show) <.> parseWithBindings'
evalOnce READ = \_ -> printLn . fmap (unparse True False) . readOutput
evalOnce TOFORMAL = (printLn . fmap unparseFormal) <.> parseWithBindings'
evalOnce TOINFORMAL = (printLn . fmap (unparse True False)) <.> parseWithBindings'
evalOnce LET = withTwo replaceChar parseBinding Content id "IN"
evalOnce WITHBIND = withTwo (flip replaceChar) Content parseBinding id "WITH"
evalOnce ASSIGN = withThree (flip . curry $ replaceChar) Content extractChar Content id ("ASSIGN TO", "IN")
  where
    extractChar :: String -> Output Char
    extractChar [] = Error "No binding name provided" []
    extractChar (char:rest)
      | null rest = Content char
      | otherwise = Error "Binding names must be single-character" []

-- ┌────────────────┐
-- │ Error handling │
-- └────────────────┘

printError :: String -> String -> InputT IO ()
printError str [] = outputStrLn $ color "1;31" "error: " ++ str ++ "."
printError str trace = outputStrLn $ color "1;31" "error: " ++ str ++ ": " ++ color "33" trace

-- ┌──────────────────────────────────┐
-- │ Command piping and mode changing │
-- └──────────────────────────────────┘

handleCommand :: String -> Bindings -> (Mode -> Bindings -> String -> Action String) -> Action String
handleCommand input bindings f = do
  let
    splitByFirstSpace :: String -> Output Int
    splitByFirstSpace [] = Error "Cannot split an empty string" []
    splitByFirstSpace (' ':_) = Content 0
    splitByFirstSpace (_:rest) = (+ 1) <$> splitByFirstSpace rest
    mn = splitByFirstSpace input
  mn >>== \n -> do
    let mode = take n input
        input' = drop (n+1) input
    if notMember mode modeMap
    then return (Error "Invalid mode" mode)
    else
      if null input'
      then return (Error "No input given" [])
      else f (modeMap ! mode) bindings input'

pipeCommand :: Mode -> Mode -> Bindings -> String -> Action String
pipeCommand basemode curmode bindings input = do
  val <- eval curmode bindings input
  val >>== evalOnce basemode bindings

eval :: Mode -> Bindings -> String -> Action String
eval mode bindings input = case input of
  [] -> return (Error "No input given" [])
  (' ':rest) -> eval mode bindings rest
  ('<':rest) -> handleCommand rest bindings (pipeCommand mode)
  ('>':rest) -> handleCommand rest bindings (`pipeCommand` mode)
  -- ('@':rest) -> handleCommand rest bindings eval
  other -> evalOnce mode bindings other

(+|) :: String -> [String] -> [String]
str +| [] = [str]
str +| arr
  | str == head arr = arr
  | otherwise = str : arr

loop :: Mode -> [String] -> Bindings -> InputT IO ()
loop mode history bindings = do
  minput <- getColoredInputLine $ getPrompt '#' (show mode)
  case minput of
    Error _ _ -> return ()
    Content [] -> loop mode history bindings
    Content "help" -> helpAction >> loop mode history bindings
    Content "bindings" -> do
      let showBinding :: Char -> String -> InputT IO ()
          showBinding name binding = outputStrLn
            $ "\ESC[0m| " ++ color "35" [name] ++ " = " ++ color "34" binding -- ]
      (sequence_ . elems $ M.mapWithKey showBinding bindings) >> loop mode history bindings
    Content "clear" -> loop mode history empty
    Content "quit" -> return ()
    Content "exit" -> return ()
    Content ":q" -> return ()
    Content ('+':rest) -> case parseBinding rest of
      Error str trace -> do
        printError str trace
        loop mode history bindings
      Content (name, binding) -> do
        mbinding <- eval RETURN bindings binding
        case mbinding of
          Error str trace -> do
            printError str trace
            loop mode history bindings
          Content truebinding -> loop mode history (insert name truebinding bindings)
    Content ('|':rest) -> action
      where
        action :: InputT IO ()
        action
          | null rest = loop RETURN history bindings
          | member rest modeMap = loop (modeMap ! rest) history bindings
          | otherwise = do
              printError "Invalid mode" rest
              loop mode history bindings
    Content ('^':rest) -> action
      where
        countDistance :: String -> Int
        countDistance ('^':rest') = 1 + countDistance rest'
        countDistance _ = 0
        removeSpaces :: String -> String
        removeSpaces [] = []
        removeSpaces (' ':rest') = removeSpaces rest'
        removeSpaces (char:rest') = char : removeSpaces rest'
        distance = countDistance rest
        cmd = drop distance rest
        action :: InputT IO ()
        action
          | distance >= length history = do
              printError "History length out of bounds" (show $ distance + 1)
              loop mode history bindings
          | null cmd = do
              moutput <- eval mode bindings (history !! distance)
              case moutput of
                Error str trace -> do
                  printError str trace
                  loop mode history bindings
                Content output -> loop mode (output +| history) bindings
          | head cmd == '+' = case tail cmd of
              [] -> printError "No name given" [] >> loop mode history bindings
              str -> case removeSpaces str of
                [char] -> loop mode history (insert char (history !! distance) bindings)
                _ -> printError "Names must be single-character" [] >> loop mode history bindings
          | not (null cmd) && notMember cmd modeMap = do
              printError "Invalid mode" cmd
              loop mode history bindings
          | otherwise = do
              let curmode = if null cmd then mode else modeMap ! cmd
              moutput <- eval curmode bindings (history !! distance)
              case moutput of
                Error str trace -> do
                  printError str trace
                  loop mode history bindings
                Content output -> loop mode (output +| history) bindings
    Content ('@':rest) -> do
      moutput <- handleCommand rest bindings eval
      case moutput of
        Error str trace -> do
          printError str trace
          loop mode history bindings
        Content output -> loop mode (output +| history) bindings
    Content input -> do
      moutput <- eval mode bindings input
      case moutput of
        Error str trace -> do
          printError str trace
          loop mode history bindings
        Content output -> loop mode (output +| history) bindings

-- ┌───────────────────┐
-- │ Building the REPL │
-- └───────────────────┘

data Options = Options {
    helpOpt :: Bool,
    modeOpt :: Mode,
    errorOpt :: Maybe String
  }

parseOptions :: [String] -> Options
parseOptions [] = Options { helpOpt = False, modeOpt = RETURN, errorOpt = Nothing }
parseOptions (opt:rest)
  | opt == "-h" || opt == "--help" = (parseOptions rest) { helpOpt = True }
  | (opt == "-m" || opt == "--mode") && not (null rest) && member mode modeMap =
    (parseOptions (tail rest)) { modeOpt = modeMap ! mode }
  | otherwise = Options { errorOpt = Just opt, helpOpt = False, modeOpt = REPEAT }
  where
    mode = head rest

helpAction :: InputT IO ()
helpAction = do
  let
    l1 = 7
    l2 = 14
    l3 = 22
    getCommandInfo :: (String, String, Mode, String) -> InputT IO ()
    getCommandInfo (k1,k2,m,desc) = outputStrLn $
      calibrateLengthPost l1 ("  *" ++ k1 ++ ",") ++
      calibrateLengthPost l2 ("*" ++ k2) ++
      calibrateLengthPost l3 ("the " ++ show m ++ " mode ") ++
      desc ++ "."
    getFlagInfo :: (String, String, String) -> InputT IO ()
    getFlagInfo (s,l,desc) = outputStrLn $ 
      calibrateLengthPost l1 s ++
      calibrateLengthPost l2 l ++
      desc ++ "."
  outputStrLn "lambda-interpreter: a command line utility for parsing and processing lambda terms."
  outputStrLn "\nflags:"
  getFlagInfo ("  -h,", "--help", "helps")
  getFlagInfo ("  -m,", "--MODE", "sets the initial MODE")
  outputStrLn "\nREPL usage: [>MODE | <MODE | @MODE | ^MODE] [LAMBDA]"
  outputStrLn "\nmode prefixes:"
  outputStrLn $ calibrateLengthPost (l1+l2) "  >MODE"
    ++ "enter the specified mode."
  outputStrLn $ calibrateLengthPost (l1+l2) "  <MODE ARG"
    ++ "run the specified mode with the given argument and pipe the result into the current MODE."
  outputStrLn $ calibrateLengthPost (l1+l2) "  @MODE ARG"
    ++ "run the specified mode with the given argument."
  outputStrLn $ calibrateLengthPost (l1+l2) "  ^MODE"
    ++ "run the specified mode with the result of the previous evaluation."
  outputStrLn $ calibrateLengthPost (l1+l2) "  +BINDING"
    ++ "create a local binding, with syntax N = BODY"
  outputStrLn "\nmodes:"
  mapM_ getCommandInfo commandList

settings :: Settings IO
settings = Settings {
  complete = noCompletion,
  historyFile = Just ".lambda-interpreter-history",
  autoAddHistory = True
}

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runInputT settings $ case opts of
    Options { errorOpt = Just str } -> printError "Unrecognized option" str
    Options { helpOpt = True } -> helpAction
    _ -> loop (modeOpt opts) [] empty
