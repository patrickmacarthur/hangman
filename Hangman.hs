{-# LANGUAGE FlexibleContexts #-}
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
  , returnBadGuesses
  , maxBadGuesses
  ) where

import Control.Monad.State
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
guessList :: (MonadState HangmanState m) => m [Char]
guessList = get >>= \state -> return $ guesses state

{- Takes the human's guess and checks it.  Returns an appropriate result and a
 - new game state. -}
guessLetter :: (MonadState HangmanState m) => Char -> m HangmanResult
guessLetter guess = get >>= helper
     where helper state
            | guess `elem` (guesses state) = return RepeatGuess
            | otherwise = addGuess guess >> get >>= helper'
           helper' state
            | guess `elem` word state = 
                solved >>= \x -> return $ 
                if x then Solved $ word state else GoodGuess
            | otherwise = incrementBadGuesses >> get >>= helper''
           helper'' state
            | badGuessCount state < maxBadGuesses = return BadGuess
            | otherwise = return $ Lost $ word state

{- Adds the guess to the list of guesses. -}
addGuess :: (MonadState HangmanState m) => Char -> m ()
addGuess guess = modify $ \state -> 
    state { guesses = sort $ guess:(guesses state) }

{- Increments the bad guess count. -}
incrementBadGuesses :: (MonadState HangmanState m) => m ()
incrementBadGuesses = modify $ \state ->
    state { badGuessCount = succ (badGuessCount state) }

{- Returns the number of bad guesses given. -}
returnBadGuesses :: (MonadState HangmanState m) => m Int
returnBadGuesses = get >>= \state -> return $ badGuessCount state

{- True if the word has been solved with the given guesses. -}
solved :: (MonadState HangmanState m) => m Bool
solved = get >>= \state -> return $
    foldr (helper $ guesses state) True (word state)
  where helper _ _ False = False
        helper guesses x True  = x `elem` guesses

{- The word with all non-guessed characters as underscores. -}
showPartialWord :: (MonadState HangmanState m) => m String
showPartialWord = get >>= \state -> return $ 
    map (helper $ guesses state) (word state)
  where helper guesses x
          | x `elem` guesses = x
          | otherwise        = '_'
