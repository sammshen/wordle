module Main where
import System.IO
import System.Exit
import System.Random
import System.Environment
import System.IO
import Data.List
import Data.Char
import GHC.RTS.Flags (ProfFlags(modSelector))

maxGuesses = 6
wordLength = 5
qwertyLine1 = "QWERTYUIOP"
qwertyLine2 = "ASDFGHJKL"
qwertyLine3 = "ZXCVBNM"

newtype Color = Color {color :: String}
green = Color "\x1b[48;5;46m"
yellow = Color "\x1b[48;5;226m"
gray = Color "\x1b[48;5;242m"
white = Color "\x1b[48;5;231m"
black = Color "\x1b[48;5;16m"
clear = Color "\x1b[0m"
clearScreen = "\x1b[2J\n"

debugLog :: String -> IO ()
debugLog s =
  let debug = False in
  if debug then putStrLn s else return ()

noRepeatLetters :: String -> Bool
noRepeatLetters word = word == nub word

--------------------------------------------------------------------------------
-- Model and Actions

data Model = Model 
    { victory    ::  Bool
    , errorMsg   ::  String
    , wordList   :: [String]
    , answer     ::  String
    , board      :: [String]
    , auxBoard   :: [String]
    , keyBoard   :: [String]
    , numGuesses ::  Int
    , numChars   ::  Int
    }

data Action = KeyPress Char

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  w <- readFile "words.txt"
  let wordList = words w
  let len = length wordList
  
  hSetBuffering stdin NoBuffering
  args <- getArgs
  case args of
    []    -> do
      answer <- randomWord wordList len
      startGame wordList answer
    [arg] -> 
      if all isDigit arg then do
        let num = read arg
        if num < 0 || num > (len - 1) 
          then invalidGameNum
          else do
            let answer = (!!) wordList num
            startGame wordList answer
      else if arg == "--how-to-play" 
        then howToPlay
      else invalidGameNum
    _     -> invalidUsage

startGame :: [String] -> String -> IO ()
startGame wordList answer = do
  let capitalizedAnswer = map toUpper answer
  let newBoard    = ["     "
                    , "     "
                    , "     "
                    , "     "
                    , "     "
                    , "     "] 
  let newAuxBoard = ["     "
                    , "     "
                    , "     "
                    , "     "
                    , "     "
                    , "     "]
  let newKeyBoard = ["          "    --10
                    , "         "     -- 9
                    , "       "]      -- 7
  let newModel = Model 
                  False             --win status
                  ""                --pending error messages
                  wordList          --word list
                  capitalizedAnswer --answer
                  newBoard          --board
                  newAuxBoard       --auxiliary board
                  newKeyBoard       --auxiliary keyboard
                  0                 --number of guesses made
                  0                 --number of characters inputted
  putStrLn "\nGuess the wordle!"
  controller newModel
  return ()

invalidUsage :: IO a
invalidUsage =
  die "Usage:\n\n\
    \  ./wordle                  Play random game\n\
    \  ./wordle gameNumber       Play specific game\n\
    \  ./wordle --how-to-play    Display instructions"

invalidGameNum :: IO a
invalidGameNum = 
  die "Invalid game number"

howToPlay :: IO ()
howToPlay = do
  putStrLn 
    "\nHOW TO PLAY\n\n\
    \Guess the WORDLE in 6 tries.\n\n\
    \Each guess must be a valid 5 letter word. Hit the enter button to submit.\n\n\
    \Examples\n"
  mapIO_ (printLetter "WEARY" "O----") [0..4]
  putStrLn "  The letter W is in the word and in the correct spot."
  mapIO_ (printLetter "PILLS" "-X---") [0..4]
  putStrLn "  The letter I is in the word but in the wrong spot."
  mapIO_ (printLetter "VAGUE" "-----") [0..4]
  putStrLn "  None of the letters are in the word in any spot."

randomWord :: [String] -> Int -> IO String
randomWord wordList len = do
  randomNum <- randomRIO (0, len - 1)
  return ((!!) wordList randomNum)

--------------------------------------------------------------------------------
-- Controller

controller :: Model -> IO Model 
controller model = do
  view model
  initChar <- getChar
  let newModel = update (KeyPress (toUpper initChar)) model
  putStr clearScreen
  hFlush stdout
  if errorMsg newModel /= "" then do
    putStrLn $ errorMsg newModel 
    controller newModel {errorMsg = ""}
  else if victory newModel then do 
    celebrate $ numGuesses newModel
    view newModel
    return newModel
  else if numGuesses newModel >= maxGuesses then do
    putStrLn ("Bummer, the answer was " ++ answer newModel)
    view newModel
    return newModel
  else controller newModel

update ::  Action -> Model -> Model
update (KeyPress c) model@(Model victory errorMsg wordList answer board 
                           auxBoard keyBoard numGuesses numChars) = do
  let currGuess = (!!) board numGuesses
  if c == '\n' then do
    if numChars < wordLength then
      model {errorMsg = "\nNot enough letters"}
    else if map toLower currGuess `notElem` wordList then
      model {errorMsg = "\nNot in word list"}
    else do 
      let auxLine = constructAuxLine currGuess answer
      let newAuxBoard = listInsert numGuesses auxBoard auxLine
      let newKeyBoard = updateKeyBoard keyBoard auxLine currGuess 0
      model { victory = currGuess == answer
            , numGuesses = numGuesses + 1
            , numChars = 0
            , auxBoard = newAuxBoard
            , keyBoard = newKeyBoard }
  else if isLetter c then do
    if numChars >= wordLength then model
    else do
      let newWord  = listInsert numChars   currGuess c
      let newBoard = listInsert numGuesses board     newWord
      model { numChars = numChars + 1
            , board = newBoard }
  else if c == '\DEL' then do
    if numChars <= 0 then model
    else do
      let newWord  = listInsert (numChars-1) currGuess ' '
      let newBoard = listInsert numGuesses board     newWord
      model { numChars = numChars - 1
            , board = newBoard }
  else model

updateKeyBoard :: [String] -> String -> String -> Int -> [String]
updateKeyBoard keyBoard _ _ 5 = keyBoard
updateKeyBoard keyBoard auxLine guess i = do
  let auxChar = (!!) auxLine i 
  let letter = (!!) guess i
  case auxChar of 
    'O'  -> do
      let newKeyBoard = updateKey keyBoard letter 'O' 
      updateKeyBoard newKeyBoard auxLine guess (i+1)
    'X'  -> do
      let newKeyBoard = updateKey keyBoard letter 'X'
      updateKeyBoard newKeyBoard auxLine guess (i+1)
    '-'  -> do 
      let newKeyBoard = updateKey keyBoard letter '-'
      updateKeyBoard newKeyBoard auxLine guess (i+1)

updateKey :: [String] -> Char -> Char -> [String]
updateKey [keyBoardLine1, keyBoardLine2, keyBoardLine3] letter newAuxChar
  | letter `elem` qwertyLine1 = do
    let (Just i) = elemIndex letter qwertyLine1
    let newkeyBoardLine1 = newKeyBoardLine keyBoardLine1 letter newAuxChar i
    [newkeyBoardLine1, keyBoardLine2, keyBoardLine3]
  | letter `elem` qwertyLine2 = do
    let (Just i) = elemIndex letter qwertyLine2
    let newkeyBoardLine2 = newKeyBoardLine keyBoardLine2 letter newAuxChar i
    [keyBoardLine1, newkeyBoardLine2, keyBoardLine3]
  | otherwise = do
    let (Just i) = elemIndex letter qwertyLine3
    let newkeyBoardLine3 = newKeyBoardLine keyBoardLine3 letter newAuxChar i
    [keyBoardLine1, keyBoardLine2, newkeyBoardLine3]
  
newKeyBoardLine :: String -> Char -> Char -> Int -> String
newKeyBoardLine keyBoardLine letter newAuxChar i = do
  let oldAuxChar = (!!) keyBoardLine i
  let auxChar = keyPrecedence oldAuxChar newAuxChar
  listInsert i keyBoardLine auxChar

keyPrecedence :: Char -> Char -> Char
keyPrecedence k1 k2
  | k1 == 'O' || k2 == 'O' = 'O'
  | k1 == 'X' || k2 == 'X' = 'X'
  | otherwise = '-'

constructAuxLine :: String -> String -> String
constructAuxLine guess answer = do
  let emptyAuxLine = "-----"
  findGreen answer guess emptyAuxLine 0

findGreen :: String -> String -> String -> Int -> String
findGreen answer guess auxLine 5 = 
  findYellow answer guess auxLine 0
findGreen answer guess auxLine i =
  if (!!) answer i == (!!) guess i
    then do
      let newAuxBoard = listInsert i auxLine 'O'
      let newAnswer = listInsert i answer '*'
      findGreen newAnswer guess newAuxBoard (i+1)
    else
      findGreen answer guess auxLine (i+1)

findYellow :: String -> String -> String -> Int -> String
findYellow answer guess auxLine 5 = auxLine
findYellow answer guess auxLine i = do
  if (!!) auxLine i == 'O' then findYellow answer guess auxLine (i+1)
  else do
    let letter = (!!) guess i
    case elemIndex letter answer of
      Just j  -> do
        let newAuxLine = listInsert i auxLine 'X'
        let newAnswer = listInsert j answer '*'
        findYellow newAnswer guess newAuxLine (i+1)
      Nothing -> 
        findYellow answer guess auxLine (i+1)

listInsert :: Int -> [a] -> a -> [a]
listInsert i list item = do
  let (x, _:ys) = splitAt i list
  case x of
    [] ->       item : ys
    _  -> x ++ (item : ys)

celebrate :: Int -> IO ()
celebrate numGuesses
  | numGuesses == 1 = putStrLn "Genius!"
  | numGuesses == 2 = putStrLn "Magnificent!"
  | numGuesses == 3 = putStrLn "Impressive!"
  | numGuesses == 4 = putStrLn "Splendid!"
  | numGuesses == 5 = putStrLn "Great!"
  | numGuesses == 6 = putStrLn "Phew!"

--------------------------------------------------------------------------------
-- View

view :: Model -> IO ()
view model = do
  printBoard model
  printKeyBoard model

printKeyBoard :: Model -> IO ()
printKeyBoard model@(Model _ _ _ _ _ _ 
                    [keyBoardLine1, keyBoardLine2, keyBoardLine3] _ _) = do
  mapIO_ (printLetter qwertyLine1 keyBoardLine1) [0..9]
  putStrLn ""
  mapIO_ (printLetter qwertyLine2 keyBoardLine2) [0..8]
  putStrLn ""
  mapIO_ (printLetter qwertyLine3 keyBoardLine3) [0..6]
  putStrLn "\n"

printBoard :: Model -> IO ()
printBoard model = do
  putStrLn $ color black ++ "                   " ++ color clear
  mapIO_ (printBoardLine model) [0..(maxGuesses-1)]
  putStrLn $ color black ++ "                   " ++ color clear

printBoardLine :: Model -> Int -> IO ()
printBoardLine model@(Model _ _ _ _ board auxBoard _ _ _) i = do
  let auxBoardLine = (!!) auxBoard i
  let boardLine = (!!) board i
  putStr $ color black ++ "  "
  mapIO_ (printLetter boardLine auxBoardLine) [0..(wordLength-1)]
  putStrLn $ color black ++ "  " ++ color clear

printLetter :: String -> String -> Int -> IO ()
printLetter charLine auxLine i = do
  let letter = toUpper ((!!) charLine i)
  case (!!) auxLine i of
    'O' -> putStr $ color green
    'X' -> putStr $ color yellow
    '-' -> putStr $ color gray
    _   -> putStr $ color white
  putStr $ " " ++ [letter] ++ " " ++ color clear

mapIO_ :: (a -> IO b) -> [a] -> IO ()
mapIO_ f []      = return ()
mapIO_ f (a: as) = do  
  f a
  mapIO_ f as
