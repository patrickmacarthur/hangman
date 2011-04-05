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
  | otherwise =
      printStatus word guesses badGuessCount >>
      putStr "Enter a guess please: " >>
      getChar >>= \guess ->
      putStrLn [] >>
      let alreadyGuessed guess = guess `elem` guesses
          badGuessCount' guess badGuessCount = 
            if guess `elem` word && (not . alreadyGuessed) guess
            then badGuessCount 
            else badGuessCount + 1
          guesses' guess = 
            if alreadyGuessed guess then guesses else sort (guess:guesses)
      in hangman word (guesses' guess) (badGuessCount' guess badGuessCount)

{- Prints the game status. -}
printStatus :: String -> [Char] -> Int -> IO ()
printStatus word guesses badGuessCount = putStrLn
    $ "The word is " ++ showPartialWord word guesses
   ++ "\nYou have guessed the following letters: " ++ intersperse ' ' guesses
   ++ "\nIncorrect guesses: " ++ show badGuessCount ++ "/" ++ show maxBadGuesses

{- True if the word has been solved with the given guesses. -}
solved :: String -> [Char] -> Bool
solved word guesses = foldr helper True word
  where helper _ False = False
        helper x True  = x `elem` guesses

{- The word with all non-guessed characters as underscores. -}
showPartialWord :: String -> [Char] -> String
showPartialWord word guesses = map helper word
  where helper x
          | x `elem` guesses = x
          | otherwise        = '_'

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
