{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module AI where
import GamePieces
import Question
import Game
-- import Tests
import Test.QuickCheck
import Data.Set as Set
import Data.Map as Map

type CardGuess = Map Card (Set Int)

initialGuess :: GameStore -> CardGuess
initialGuess store = let playerIds = Map.keysSet (players store) in
  Prelude.foldr (\c m -> Map.insert c playerIds m) Map.empty deck

updateCardGuess :: CardGuess -> Card -> Int -> CardGuess
updateCardGuess cg card pid = adjust (Set.delete pid) card cg 

-- | claims ranks until there are none left to claim
claimAllPossibleRanks :: GameStore -> Player -> Rank -> Maybe GameStore
claimAllPossibleRanks = undefined

-- | get random card based on seed
getRandomCard :: Int -> Card
getRandomCard i = let (card, _) = (random :: Card) in card
--random :: RandomGen g => g -> (a, g)

-- | claims all possible ranks, asks specific card question,
-- | updates store
runTurn :: (Input m, Output m) => [Int] -> GameStore -> m ()
runTurn = undefined


