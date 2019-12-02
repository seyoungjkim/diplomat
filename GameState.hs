{-# OPTIONS -Wincomplete-patterns #-}
module GameState where

import State (State, get, put)
import Data.Set as Set
import Data.Map as Map
import System.Random
import Text.Read

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

data Player = P { pid :: Int, hand :: PlayerHand, ranks :: Set Rank, ai :: Bool }
  deriving (Eq, Show)

type PlayerHand = Set Card

-- Game represented as a GameStore and current player
type Game = State GameStore [Player]

data GameStore = G { players :: [Player], faceUpCards :: [Card] }

instance Show GameStore where
  show = undefined

-------------------------------------------------------------------------------
plusCard :: Card -> Int -> Int
plusCard c i = fromEnum (rank c) + 1 + i

multiplyCard :: Card -> Int -> Int
multiplyCard c i = (fromEnum (rank c) + 1) * i

-- | initializes game given the number of total players and AIs
initialGameStore :: Int -> Int -> GameStore
initialGameStore n a =
  let n' = n - a
      pids = [0..n' - 1]
      aids = [n'..n - 1]
      hands = dealDeck n deck
      players = createPlayers pids (Prelude.take n' hands) False
      ais = createPlayers aids (Prelude.drop n' hands) True in
  G (players ++ ais) []
  where
    createPlayers :: [Int] -> [PlayerHand] -> Bool -> [Player]
    createPlayers (id : ids) (h : hands) b = 
      (P id h Set.empty b) : createPlayers ids hands b
    createPlayers _ _ _ = []
    createCycle :: [Player] -> [Player]
    createCycle ps = ps ++ createCycle ps

-- | checks if any player has won the game
checkEnd :: GameStore -> Bool
checkEnd gs = check [Ace ..] (players gs) where
  check :: [Rank] -> [Player] -> Bool
  check [] _ = True
  check (r:rs) players =
    Prelude.foldr (\p acc -> Set.member r (ranks p) || acc) False players && check rs players

-- -- | Automatically lets current player claim ranks
-- claimRank :: Player -> GameStore -> GameStore
-- claimRank = undefined

-- | creates a deck of 52 cards
deck :: [Card]
deck = do
       r <- [Ace ..]
       s <- [Diamond ..]
       return $ Card r s

-- | shuffles a deck
shuffleDeck :: [Card] -> [Card]
shuffleDeck d = let (d', _) = fisherYates (mkStdGen 0) d in d'

-- | shuffle and deal deck to given number of players
dealDeck :: Int -> [Card] -> [PlayerHand]
dealDeck n cs = let d = 52 `div` n
                    m = 52 `mod` n 
                    r = m * (d + 1)
                    cs' = shuffleDeck cs in
  if m > 0 
    then (deal (d + 1) (Prelude.take r cs')) ++ (deal d (Prelude.drop r cs'))
  else deal d cs'
  where
    deal _ [] = []
    deal n l = let (hd, tl) = Prelude.splitAt n l in
      (Set.fromList hd) : (deal n tl)

-- shuffle deck helper functions
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ Prelude.foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)