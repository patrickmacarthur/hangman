-- File: Hangman.hs
-- Author: Patrick MacArthur

{- A simple interactive hangman game. -}

import Control.Monad
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
  | badGuessCount >= maxBadGuesses =
      putStrLn $ "You ran out of guesses.  The word is " ++ word ++ "."
  | otherwise = do
      putStrLn $ "The word is " ++ showPartialWord word guesses
      putStrLn "Enter a guess please: "
      guess <- getChar
      putStrLn []
      hangman word (guess:guesses)
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

{- Returns a word in the given dictionary file that contains only characters in
 - the given list of valid characters. -}
getDictionaryWord :: String -> [Char] -> IO String
getDictionaryWord dictFile valid = do
    contents <- readFile dictFile
    getRandomWord $ (filter isValidWord . lines) contents
  where isValidWord = foldr isValidChar True
        isValidChar _ False = False
        isValidChar x _     = x `elem` valid

{- Returns a random word in the given list of words. -}
getRandomWord :: [String] -> IO String
getRandomWord allWords =
  getStdRandom (randomR (0,length allWords)) >>= \index ->
    return $ allWords !! index

main :: IO ()
main = do 
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    word <- getDictionaryWord "/usr/share/dict/words" validChars
    hangman word [] 0
  where validChars = "abcdefghijklmnopqrstuvwxyz"
