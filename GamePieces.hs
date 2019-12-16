{-# OPTIONS -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Werror #-}

module GamePieces where

import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Data.Map (Map)
import qualified Data.Map as Map (insert, elems, singleton, findWithDefault)
import System.Random (RandomGen, randomR, mkStdGen)
import Text.Read ()

-------------------------------------------------------------------------------
data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Ord, Read)

instance Show Card where
  show c = show (rank c) ++ " of " ++ show (suit c) ++ "s"

data Rank = Ace   | Two  | Three | Four | Five  | Six | Seven 
          | Eight | Nine | Ten   | Jack | Queen | King
  deriving (Eq, Show, Ord, Enum, Bounded, Read)

data Suit = Diamond | Club | Heart | Spade
  deriving (Eq, Show, Ord, Enum, Bounded, Read)

-- Stores info on a player, including if they are an AI 
-- and the AI's current guess state
data Player = 
  P { pid :: Int, hand :: PlayerHand, ranks :: Set Rank, 
      ai :: Bool, aiGuess :: CardGuess }
  deriving (Eq, Show)

type PlayerHand = Set Card

-- Used by AI to keep track of possible owners of cards
type CardGuess = Map Card (Set Int)

-------------------------------------------------------------------------------
-- | adds rank of card to the given int
plusCard :: Card -> Int -> Int
plusCard c i = fromEnum (rank c) + 1 + i

-- | multiplies rank of card with the given int
multiplyCard :: Card -> Int -> Int
multiplyCard c i = (fromEnum (rank c) + 1) * i

-- | creates a deck of 52 cards
deck :: [Card]
deck = do
  r <- [Ace ..]
  Card r <$> [Diamond .. ]

-- | shuffle and deal deck to given number of players
dealDeck :: Int -> Int -> [Card] -> [PlayerHand]
dealDeck seed n cs = 
  let d = 52 `div` n
      m = 52 `mod` n 
      r = m * (d + 1)
      cs' = shuffleDeck seed cs in
  if m > 0 
    then deal (d + 1) (Prelude.take r cs') ++ deal d (Prelude.drop r cs')
  else deal d cs'
  where
    deal _ [] = []
    deal n l = let (hd, tl) = Prelude.splitAt n l in
      Set.fromList hd : deal n tl

-- | shuffles a deck
shuffleDeck :: Int -> [Card] -> [Card]
shuffleDeck seed deck = 
  let (deck', _) = fisherYates (mkStdGen seed) deck in deck'

-- | shuffle deck helper functions
fisherYatesStep :: RandomGen g => 
  (Map Int Card, g) -> (Int, Card) -> (Map Int Card, g)
fisherYatesStep (m, gen) (i, x) =
  ((Map.insert j x . Map.insert i 
    (Map.findWithDefault defaultCard j m)) m, gen')
  where
    (j, gen') = randomR (0, i) gen
    defaultCard = Card { rank = Ace, suit = Heart }

-- | shuffle deck helper functions
fisherYates :: RandomGen g => g -> [Card] -> ([Card], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ 
  Prelude.foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)


