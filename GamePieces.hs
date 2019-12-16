{-# OPTIONS -Wincomplete-patterns #-}
module GamePieces where

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

data Player = 
  P { pid :: Int, hand :: PlayerHand, ranks :: Set Rank, ai :: Bool, aiGuess :: CardGuess }
  deriving (Eq, Show)

type CardGuess = Map Card (Set Int)

type PlayerHand = Set Card

-------------------------------------------------------------------------------
plusCard :: Card -> Int -> Int
plusCard c i = fromEnum (rank c) + 1 + i

multiplyCard :: Card -> Int -> Int
multiplyCard c i = (fromEnum (rank c) + 1) * i

-- | creates a deck of 52 cards
deck :: [Card]
deck = do
       r <- [Ace ..]
       Card r <$> [Diamond .. ]

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
    then deal (d + 1) (Prelude.take r cs') ++ deal d (Prelude.drop r cs')
  else deal d cs'
  where
    deal _ [] = []
    deal n l = let (hd, tl) = Prelude.splitAt n l in
      Set.fromList hd : deal n tl

-- shuffle deck helper functions
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = 
  ((Map.insert j x . Map.insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ 
  Prelude.foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)



-------------------------------- Strings in Game ------------------------------

instructions :: String
instructions = "=== INSTRUCTIONS ===\n\
               \Diplomat is very similar to the popular card game, Go Fish.\n\
               \Cards are distributed evenly among all players, and then \n\
               \players take turns questioning each other. \n\nThe goal of \
               \the game is to collect as many ranks as possible,\nwhere \
               \claiming a rank means collecting all four cards of the\nrank \
               \(i.e. you can claim the Ace rank when you've located all\n\
               \four Aces in the game). \n\n\
               \On your turn, you can do the following actions:\n\n\
               \  1. Ask another player if they have a specific card (i.e.\n\
               \  Ace of Spades). If the anwer is yes, this card is laid out\n\
               \  and all players can see it, and you can continue your turn\n\
               \  and ask another question to any player. If the answer is\n\
               \  No, your turn ends.\n\n\
               \  2. Ask another player any Yes-No question about their hand\n\
               \  (i.e. 'Do you have any Black Queens?'). Regardless if the\n\
               \  answer is Yes or No, your turn ends.\n\n\
               \At any point during your turn, you can claim a rank if you\n\
               \know where all four cards of a rank are (AKA all four cards\n\
               \are either in your hand or laid out).\n\
               \====================\n"

commandsText :: String
commandsText = "Here are commands you can use:\n \
           \help: to view what commands you can use\n \
           \instr: to view Diplomat instructions\n \
           \hand: to view the cards in your hand\n \
           \laidout: to view currently laid-out cards\n \
           \claimed: to view the ranks you have claimed\n \
           \claim: to claim a rank\n \
           \ask: to ask another player a question\n \
           \quit: to quit the game.\n"

introText :: String
introText = "\nWelcome to Diplomat!\n"

promptText :: String
promptText = ">> Please type a command."

summaryText :: String
summaryText = "Summary since your last move:"

claimHelpText :: String
claimHelpText = "Here are commands you can use:\n \
                \help: to view what commands you can use\n \
                \nvm: to stop claiming a rank. \
                \quit: to quit the game.\n"