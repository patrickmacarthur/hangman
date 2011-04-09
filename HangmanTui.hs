-- File: HangmanTui.hs

import Hangman
import Control.Monad
import Data.Char
import Data.List
import System.Random
import System.IO

{- Main hangman routine. -}
hangman :: HangmanState -> IO ()
hangman state =
  printStatus (showPartialWord state) (guessList state) (badGuessCount state) >>
  getGuess >>= \guess ->
  case guessLetter state guess of
    (Solved word, _) -> putStrLn $ "You solved it!  The word is " ++ word ++ "!"
    (Lost word, _) -> putStrLn $ "Sorry, you lost.  The word is " ++ word ++ "."
    (_, state') -> hangman state'

getGuess :: IO Char
getGuess = do
    putStr "Enter a guess please: "
    guess <- getChar
    putStrLn []
    return guess

{- Prints the game status. -}
printStatus :: HangmanState -> IO ()
printStatus state = putStrLn
    $ "The word is " ++ showPartialWord state
   ++ "\nYou have guessed the following letters: "
   ++ intersperse ' ' (guessList state)
   ++ "\nIncorrect guesses: " 
   ++ show (badGuessCount state) ++ "/" ++ show maxBadGuesses

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
          hangman $ newGame word
          putStr "Play again? (y/n): "
          response <- getChar
          putStrLn "\n"
          when (toLower response == 'y') $ gameWrapper allWords
