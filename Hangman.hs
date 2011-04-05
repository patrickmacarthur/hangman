-- File: Hangman.hs
-- Author: Patrick MacArthur

{- A simple interactive hangman game. -}

import Control.Monad
import Data.Char
import Data.List
import System.Random
import System.IO

{- Maximum number of bad guesses before we end the game. -}
maxBadGuesses = 7

{- Main hangman routine. -}
hangman :: String -> [Char] -> Int -> IO ()
hangman word guesses badGuessCount
  | solved word guesses = 
      putStrLn $ "You solved it!  The word is " ++ word ++ "!"
  | badGuessCount > maxBadGuesses =
      putStrLn $ "You ran out of guesses.  The word is " ++ word ++ "."
  | otherwise = do
      putStrLn $ "The word is " ++ showPartialWord word guesses
      putStrLn $ "You have guessed the following letters: " ++ 
                   (intersperse ' ' . sort) guesses
      putStr "Incorrect guesses: "
      putStrLn $ show badGuessCount ++ "/" ++ show maxBadGuesses
      putStr "Enter a guess please: "
      guess <- getChar
      putStrLn []
      if guess `elem` guesses
        then hangman word guesses badGuessCount
        else hangman word (guess:guesses)
          (if guess `elem` word then badGuessCount else badGuessCount + 1)

{- True if the word has been solved with the given guesses. -}
solved :: String -> [Char] -> Bool
solved word guesses = helper word
  where helper [] = True
        helper (x:xs)
          | x `elem` guesses = helper xs
          | otherwise        = False

{- The word with all non-guessed characters as underscores. -}
showPartialWord :: String -> [Char] -> String
showPartialWord [] _ = []
showPartialWord word guesses = helper word
  where helper [] = []
        helper (x:xs)
          | x `elem` guesses = x : helper xs
          | otherwise        = '_' : helper xs

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
          hangman word [] 0
          putStr $ "Play again? (y/n): "
          response <- getChar
          putStrLn "\n"
          when (toLower response == 'y') $ gameWrapper allWords
