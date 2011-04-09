-- File: HangmanTui.hs

import Hangman
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Char
import Data.List
import System.Random
import System.IO

{- Main hangman routine. -}
hangman :: StateT HangmanState IO ()
hangman =
  printStatus >>
  liftIO getGuess >>= 
  guessLetter >>= \result ->
  case result of
    Solved word -> liftIO $ putStrLn $ "You solved it!  The word is " ++ word ++ "!"
    Lost word -> liftIO $ putStrLn $ "Sorry, you lost.  The word is " ++ word ++ "."
    _ -> hangman

getGuess :: IO Char
getGuess = do
    putStr "Enter a guess please: "
    guess <- getChar
    putStrLn []
    return guess

{- Prints the game status. -}
printStatus :: StateT HangmanState IO ()
printStatus = do
  showPartialWord >>= \word -> liftIO $ putStrLn $
    "The word is " ++ word
  guessList >>= \list -> liftIO $ putStrLn $
    "\nYou have guessed the following letters: " ++ intersperse ' ' list
  returnBadGuesses >>= \count -> liftIO $ putStrLn $
    "\nIncorrect guesses: " ++ show count ++ "/" ++ show maxBadGuesses

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

main :: IO ()
main = do 
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    allWords <- getDictionaryWords "/usr/share/dict/words" isValidWord
    gameWrapper allWords
  where gameWrapper allWords = do
          word <- getRandomWord allWords
          runStateT hangman $ newGame word
          putStr "Play again? (y/n): "
          response <- getChar
          putStrLn "\n"
          when (toLower response == 'y') $ gameWrapper allWords
