-- File: Hangman.hs

import Prelude hiding (catch)

import HangmanState
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Char
import Data.List
import System.Random
import System.IO
import System.IO.Error

{- Main hangman routine. -}
hangman :: StateT HangmanState IO ()
hangman =
  printStatus >>
  liftIO getGuess >>= 
  guessLetter >>= \result ->
  case result of
    Solved word -> lPutStrLn $ "You solved it!  The word is " ++ word ++ "!"
    Lost word -> lPutStrLn $ "Sorry, you lost.  The word is " ++ word ++ "."
    BadGuess -> lPutStrLn "Incorrect guess!" >> hangman
    RepeatGuess -> lPutStrLn "You already guessed that!" >> hangman
    GoodGuess -> lPutStrLn "Ok." >> hangman

getGuess :: IO Char
getGuess = do
    putStr "Enter a guess please: "
    hFlush stdout
    guessLine <- getLine
    putStrLn []
    if length guessLine /= 1
            then putStrLn "Please enter a single letter!" >> getGuess
            else return $ head guessLine

{- Prints the game status. -}
printStatus :: StateT HangmanState IO ()
printStatus = do
  showPartialWord >>= \word -> lPutStrLn $
    "The word is " ++ word
  guessList >>= \list -> lPutStrLn $
    "You have guessed the following letters: " ++ intersperse ' ' list
  returnBadGuesses >>= \count -> lPutStrLn $
    "Incorrect guesses: " ++ show count ++ "/" ++ show maxBadGuesses

{- Returns all words in the given dictionary file that contains only characters
 - that satisfy the given condition. -}
getDictionaryWords :: String -> (String -> Bool) -> IO [String]
getDictionaryWords dictFile valid = do
    contents <- readFile dictFile
    return $ (filter valid . lines) contents

{- Returns a random word in the given list of words. -}
getRandomWord :: [String] -> IO String
getRandomWord allWords =
  getStdRandom (randomR (0,length allWords)) >>= \index ->
    return $ allWords !! index

{- Predicate to determine if a word is valid for use in the hangman game. -}
isValidWord :: String -> Bool
isValidWord word =
       foldr isValidChar True word 
    && lengthWord > lengthMin 
    && lengthWord < lengthMax
  where isValidChar _ False = False
        isValidChar x True  = isLower x
        lengthWord = length word
        lengthMin  = 6
        lengthMax  = 12

{- Helper function that lifts putStrLn into the State monad for us -}
lPutStrLn :: String -> StateT HangmanState IO ()
lPutStrLn s = liftIO $ putStrLn s

main :: IO ()
main = do 
    allWords <- getDictionaryWords "/usr/share/dict/words" isValidWord
    handle (\e -> if isEOFError e then putStrLn [] else ioError e) $
        gameWrapper allWords
  where gameWrapper allWords = do
          word <- getRandomWord allWords
          runStateT hangman $ newGame word
          putStr "Play again? (y/n): "
          hFlush stdout
          response <- getLine
          putStrLn "\n"
          when (map toLower response == "y") $ gameWrapper allWords
