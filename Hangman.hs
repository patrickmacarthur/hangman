-- File: Hangman.hs
-- Author: Patrick MacArthur

{- A simple interactive hangman game. -}

module Hangman
  ( HangmanResult(..)
  , HangmanState()
  , newGame
  , guessLetter
  , showPartialWord
  , guessList
  , badGuessCount
  , maxBadGuesses
  ) where

import Data.Char
import Data.List

data HangmanResult = Solved String | Lost String | GoodGuess | BadGuess | RepeatGuess

data HangmanState = HangmanState { word          :: String
                                 , guesses       :: [Char]
                                 , badGuessCount :: Int
                                 }

{- Maximum number of bad guesses before we end the game. -}
maxBadGuesses = 7

{- An initial state for a hangman game, given a word. -}
newGame :: String -> HangmanState
newGame w = HangmanState { word = w, guesses = [], badGuessCount = 0 }

{- Return the list of guesses so far. -}
guessList :: HangmanState -> [Char]
guessList state = guesses state

{- Takes the human's guess and checks it.  Returns an appropriate result and a
 - new game state. -}
guessLetter :: HangmanState -> Char -> (HangmanResult, HangmanState)
guessLetter state guess 
  | guess `elem` (guesses state) = (RepeatGuess, state)
  | otherwise = makeResult
     where state' = state {guesses = sort $ guess:(guesses state)}
           stateBad = state' { badGuessCount = succ (badGuessCount state) }
           makeResult
            | guess `elem` word state' =
              ( if solved (word state') (guesses state') 
                then Solved (word state') 
                else GoodGuess
              , state'
              )
            | badGuessCount state < maxBadGuesses = (BadGuess, stateBad)
            | otherwise = (Lost (word state), stateBad)

{- True if the word has been solved with the given guesses. -}
solved :: String -> [Char] -> Bool
solved word guesses = foldr helper True word
  where helper _ False = False
        helper x True  = x `elem` guesses

{- The word with all non-guessed characters as underscores. -}
showPartialWord :: HangmanState -> String
showPartialWord state = map helper (word state)
  where helper x
          | x `elem` (guesses state) = x
          | otherwise                = '_'
