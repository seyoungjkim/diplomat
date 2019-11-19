{-# OPTIONS -Wincomplete-patterns #-}
module GameState (Card, Suit, Rank, Player, Game, Hand, deck) where

import State (State)
import Data.Set as Set

-------------------------------------------------------------------------------
data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Show, Ord)

data Rank = Ace   | Two  | Three | Four | Five  | Six | Seven 
          | Eight | Nine | Ten   | Jack | Queen | King
  deriving (Eq, Show, Ord, Enum, Bounded)

data Suit = Diamond | Club | Heart | Spade
  deriving (Eq, Show, Ord, Enum, Bounded)

data Player = P { id :: Integer, hand :: Hand, ranks :: Set Rank, ai :: Bool }
  deriving (Eq, Show)

type Hand = Set Card

-- Game represented as a GameStore and current player
type Game = State GameStore Player

data GameStore = G { players :: [Player], faceUpCards :: [Card] }

-- Way to represent the game state to the player.
show :: GameStore -> String
show = undefined

-- | initializes game
initialGame :: Game
initialGame = undefined

-- | checks if any player has won the game
checkEnd :: Game -> Bool
checkEnd = undefined

-- | Automatically lets current player claim ranks
claimRank :: Player -> GameStore -> GameStore
claimRank = undefined

-- | creates a deck of 52 cards
deck :: [Card]
deck = do
       r <- [Ace ..]
       s <- [Diamond ..]
       return $ Card r s