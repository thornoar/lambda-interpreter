module Main where

import Lambda
import System.Console.Haskeline
import System.Environment (getArgs)
import Data.Map (Map, empty, insert, member, notMember, (!), elems)
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
  | LET | WHERE | ASSIGN
  deriving (Read, Show)

type Action = InputT IO (Maybe String)
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
    ("wh", "where", WHERE, "allows to set a binding, order (expression, binding)"),
    ("as", "assign", ASSIGN, "a three-step action similar to LET and WHERE, but allows piping into bindings")
  ]

commandMap :: Map String Mode
commandMap = mapFromList commandList

-- ┌────────────────────────────────┐
-- │ Input/Output string processing │
-- └────────────────────────────────┘

(>>==) :: (Monad m) => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
(>>==) Nothing _ = return Nothing
(>>==) (Just a) f = f a

infixr 9 <.>
(<.>) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(<.>) g f a b = g (f a b)

splitByFirstSpace :: String -> Maybe Int
splitByFirstSpace [] = Nothing
splitByFirstSpace (' ':_) = Just 0
splitByFirstSpace (_:rest) = (+ 1) <$> splitByFirstSpace rest

calibrateLengthPost :: Int -> String -> String
calibrateLengthPost n str
  | len > n = str
  | otherwise = str ++ replicate (n - len) ' '
  where
    len = length str

calibrateLengthPre :: Int -> Char -> String -> String
calibrateLengthPre n rep str
  | len > n = str
  | otherwise = replicate (n - len) rep ++ str
  where
    len = length str

replaceChar :: (Char, String) -> String -> String
replaceChar _ [] = []
replaceChar (from, to) (char:rest)
  | char == from = "(" ++ to ++ ")" ++ replaceChar (from, to) rest
  | otherwise = char : replaceChar (from, to) rest

parseBinding :: String -> Maybe (Char, String)
parseBinding [] = Nothing
parseBinding (' ':rest) = parseBinding rest
parseBinding (char:' ':rest) = parseBinding (char:rest)
parseBinding (char:'=':' ':rest) = parseBinding (char:'=':rest)
parseBinding (char:'=':rest)
  | null rest = Nothing
  | otherwise = Just (char, rest)
parseBinding _ = Nothing
      -- | '=' /= head rest = Nothing
      -- | otherwise = Just (char, tail rest)

useBindings :: Bindings -> String -> String
useBindings _ [] = []
useBindings bindings (char:rest)
  | char `member` bindings = "(" ++ (bindings ! char) ++ ")" ++ useBindings bindings rest
  | otherwise = char : useBindings bindings rest

parseWithBindings' :: Bindings -> String -> Maybe Lambda
parseWithBindings' bindings = parse' . useBindings bindings

color :: String -> String -> String
color typ str = "\ESC[" ++ typ ++ "m" ++ str ++ "\ESC[0m" -- ]]

promptLength :: Int
promptLength = 16

getPrompt :: Char -> String -> String
getPrompt rep body = "\ESC[0m(" ++ color "33" body ++ ") " ++ replicate n rep ++ ": \ESC[34m" -- ]]
  where n = promptLength - length body - 5

getColoredInputLine :: String -> Action
getColoredInputLine pref = do
  res <- getInputLine pref
  outputStr "\ESC[0m" -- ]
  return res

-- ┌─────────────────────────────────┐
-- │ Evaluating and returning output │
-- └─────────────────────────────────┘

printGeneral :: (String -> InputT IO ()) -> Maybe String -> Action
printGeneral _ Nothing = return Nothing
printGeneral f (Just str) = do
  f . ("| " ++) . color "34" $ str
  return $ Just str

printLn :: Maybe String -> Action
printLn = printGeneral outputStrLn

withTwo ::
  (a -> b -> c) ->
  (String -> Maybe a) ->
  (String -> Maybe b) ->
  (c -> String) ->
  String ->
  Bindings ->
  String ->
  Action
withTwo f readA readB showC prompt bindings str1 = (readA . useBindings bindings) str1 >>== \a -> do
  minput <- getColoredInputLine $ getPrompt ' ' prompt
  mstr2 <- minput >>== eval RETURN bindings
  let mb = mstr2 >>= (readB . useBindings bindings)
  mb >>== \b -> printLn $ Just $ showC $ f a b

withThree ::
  (a -> b -> c -> d) ->
  (String -> Maybe a) ->
  (String -> Maybe b) ->
  (String -> Maybe c) ->
  (d -> String) ->
  (String, String) ->
  Bindings ->
  String ->
  Action
withThree f readA readB readC showC (prompt1, prompt2) bindings str1 =
  (readA . useBindings bindings) str1 >>== \a -> do
    minput2 <- getColoredInputLine $ getPrompt ' ' prompt1
    mstr2 <- minput2 >>== eval RETURN bindings
    let mb = mstr2 >>= (readB . useBindings bindings)
    mb >>== \b -> do
      minput3 <- getColoredInputLine $ getPrompt ' ' prompt2
      mstr3 <- minput3 >>== eval RETURN bindings
      let mc = mstr3 >>= (readC . useBindings bindings)
      mc >>== \c -> printLn $ Just $ showC $ f a b c

evalOnce :: Mode -> Bindings -> String -> Action
evalOnce RETURN = \_ -> return . Just
evalOnce REPEAT = \_ -> printLn . Just
evalOnce PRINT = (printLn . fmap (unparse True)) <.> parseWithBindings'
evalOnce EXPAND = (printLn . fmap (unparse False)) <.> parseWithBindings'
evalOnce SUBS = withThree
  substitute
  parse'
  getVar
  parse'
  (unparse True)
  ("VAR", "EXPR")
  where
    getVar :: String -> Maybe Int
    getVar str =
      let mexpr = parse' str
       in case mexpr of
          Just (Var n) -> Just n
          _ -> Nothing
evalOnce REDUCE = (printLn . fmap (unparse True . reduce)) <.> parseWithBindings'
evalOnce REDUCELIMIT = withTwo
  (flip $ reduceWithLimit 0)
  parse'
  readMaybe
  (unparse True)
  "LIMIT"
evalOnce STEPS = print' <.> parseWithBindings'
  where
    print' :: Maybe Lambda -> Action
    print' Nothing = return Nothing
    print' (Just l) = showSteps l
    showSteps :: Lambda -> Action
    showSteps l = do
      let lstr = unparse False l
      _ <- printGeneral outputStr (Just lstr)
      input <- getColoredInputLine ""
      case input of
        Nothing -> return (Just lstr)
        _ -> do
          let (l', found) = reduceStep l
          if found
          then showSteps l'
          else evalOnce PRINT empty lstr
evalOnce CONGR = withTwo congr parse' parse' show "AND"
evalOnce EQUIV = withTwo equiv parse' parse' show "AND"
evalOnce SHOW = (printLn . fmap show) <.> parseWithBindings'
evalOnce READ = \_ -> printLn . fmap (unparse True) . readMaybe
evalOnce TOFORMAL = (printLn . fmap unparseFormal) <.> parseWithBindings'
evalOnce TOINFORMAL = (printLn . fmap (unparse True)) <.> parseWithBindings'
evalOnce LET = withTwo replaceChar parseBinding Just id "IN"
evalOnce WHERE = withTwo (flip replaceChar) Just parseBinding id "WITH"
evalOnce ASSIGN = withThree (flip . curry $ replaceChar) Just extractChar Just id ("ASSIGN TO", "IN")
  where
    extractChar :: String -> Maybe Char
    extractChar [] = Nothing
    extractChar (char:rest)
      | null rest = Just char
      | otherwise = Nothing

-- ┌────────────────┐
-- │ Error handling │
-- └────────────────┘

errorString :: String -> String
errorString str = color "1;31" "error: " ++ str ++ "."

printError :: String -> InputT IO ()
printError str' = outputStrLn $ errorString str'

printInputError :: InputT IO ()
printInputError = printError "Incorrect input"

-- ┌──────────────────────────────────┐
-- │ Command piping and mode changing │
-- └──────────────────────────────────┘

handleCommand :: String -> Bindings -> (Mode -> Bindings -> String -> Action) -> Action
handleCommand input bindings f = do
  let mn = splitByFirstSpace input
  mn >>== \n -> do
    let cmd = take n input
        input' = drop (n+1) input
    if notMember cmd commandMap || null input'
    then return Nothing
    else f (commandMap ! cmd) bindings input'

pipeCommand :: Mode -> Mode -> Bindings -> String -> Action
pipeCommand basemode curmode bindings input = do
  val <- eval curmode bindings input
  val >>== evalOnce basemode bindings

eval :: Mode -> Bindings -> String -> Action
eval mode bindings input = case input of
  [] -> return Nothing
  (' ':rest) -> eval mode bindings rest
  ('<':rest) -> handleCommand rest bindings (pipeCommand mode)
  ('|':rest) -> handleCommand rest bindings (`pipeCommand` mode)
  ('@':rest) -> handleCommand rest bindings eval
  input' -> evalOnce mode bindings input'

loop :: Mode -> String -> Bindings -> InputT IO ()
loop mode pvoutput bindings = do
  minput <- getColoredInputLine $ getPrompt '#' (show mode)
  case minput of
    Nothing -> return ()
    Just [] -> loop mode pvoutput bindings
    Just "help" -> helpAction >> loop mode pvoutput bindings
    Just "bindings" -> do
      let showBinding :: Char -> String -> InputT IO ()
          showBinding name binding = outputStrLn
            $ "\ESC[0m| " ++ color "35" [name] ++ " = " ++ color "34" binding -- ]
      (sequence_ . elems $ M.mapWithKey showBinding bindings) >> loop mode pvoutput bindings
    Just "quit" -> return ()
    Just "exit" -> return ()
    Just "q" -> return ()
    Just ('+':rest) -> do
      case parseBinding rest of
        Nothing -> do
          printError "Incorrect binding"
          loop mode pvoutput bindings
        Just (name, binding) -> loop mode pvoutput (insert name binding bindings)
    Just ('>':rest) ->
      if member rest commandMap
      then loop (commandMap ! rest) pvoutput bindings
      else do
        printError "Unknown command"
        loop mode pvoutput bindings
    Just ('^':rest) ->
      if member rest commandMap
      then do
        mmem' <- eval (commandMap ! rest) bindings pvoutput
        case mmem' of
          Nothing -> do
            printError "Exception while processing input"
            loop mode pvoutput bindings
          Just mem' -> loop mode mem' bindings
      else do
        printError "Unknown command"
        loop mode pvoutput bindings
    Just input -> do
      mmem' <- eval mode bindings input
      case mmem' of
        Nothing -> do
          printError "Exception while processing input"
          loop mode pvoutput bindings
        Just mem' -> loop mode mem' bindings

-- ┌───────────────────┐
-- │ Building the REPL │
-- └───────────────────┘

data Options = Options {
    helpOpt :: Bool,
    modeOpt :: Mode,
    errorOpt :: Bool
  }

parseOptions :: [String] -> Options
parseOptions [] = Options { helpOpt = False, modeOpt = REPEAT, errorOpt = False }
parseOptions (opt:rest)
  | opt == "-h" || opt == "--help" = (parseOptions rest) { helpOpt = True }
  | (opt == "-m" || opt == "--mode") && not (null rest) && member mode commandMap =
    (parseOptions (tail rest)) { modeOpt = commandMap ! mode }
  | otherwise = Options { errorOpt = True, helpOpt = False, modeOpt = REPEAT }
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
    Options { errorOpt = True } -> printError "Unrecognized option"
    Options { helpOpt = True } -> helpAction
    _ -> loop (modeOpt opts) "" empty
