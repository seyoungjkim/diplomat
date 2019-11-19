{-# OPTIONS -Wincomplete-patterns #-}
module Question (Question, specificQuestion, getAnswer) where
import qualified State as S
import GameState

import qualified Data.Maybe as Maybe

-------------------------------------------------------------------------
data Question =
  Q FilterQuestion AskQuestion
  | Union Question Question
  | Intersection Question Question 
  | Negation Question
  deriving (Eq, Show)

-- FilterQuestion is type that goes hand to hand
data FilterQuestion = 
  FilterId
  | Rank
  | Suit
  | GreaterThan
  | LessThan
  | FilterUnion FilterQuestion FilterQuestion
  | FilterIntersection FilterQuestion FilterQuestion
  deriving (Eq, Show)

-- AskQuestion goes from hand to boolean
data AskQuestion = 
  AskId
  | Mod
  | Sum
  | Prod
  | Multiplicity
  | AskUnion AskQuestion AskQuestion
  | AskIntersection AskQuestion AskQuestion
  deriving (Eq, Show)

-- | responds to a question about another player's hand
getAnswer :: Question -> Hand -> Bool
getAnswer = undefined

-- Check if question is of the form: do you have the _ of _?
isSpecificCard :: Question -> Bool
isSpecificCard = undefined

-- Used when player is attempting to lay out a card 
-- i.e. asking about 1 card in particular
specificQuestion :: Card -> Question
specificQuestion = undefined
