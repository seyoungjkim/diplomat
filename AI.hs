{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module AI where
import GamePieces
import Question
import Game
import Data.Set as Set
import Data.Map as Map

type CardGuess = Map Card (Set Int)

-- | initial map of all possible card assignments
initialGuess :: GameStore -> CardGuess
initialGuess store = let playerIds = Map.keysSet (players store) in
  Prelude.foldr (\c m -> Map.insert c playerIds m) Map.empty deck

-- | removes a player id from possible card guesses. if no players remain to be
-- | guessed, then card is removed from the map of possible guesses.
updateCardGuess :: CardGuess -> Card -> Int -> CardGuess
updateCardGuess cg card pid =
  let f x = if Set.null x' then Nothing else Just x' where x' = Set.delete pid x in
    Map.update f card cg

-- | attempts to claim all possible ranks
claimAllPossibleRanks :: GameStore -> Player -> GameStore
claimAllPossibleRanks store player = claim store player [Ace ..] where
  claim :: GameStore -> Player -> [Rank] -> GameStore
  claim store player [] = store
  claim store player (r:rs) = case claimRank store player r of
    Nothing -> claim store player rs
    Just store' -> claim store' player rs

-- | choose a card and player to ask about (first pair found in map)
getRandomCard :: CardGuess -> Maybe (Card, Int)
getRandomCard = Map.foldrWithKey (\k x _ -> Set.lookupMin x >>= (\x' -> Just (k, x'))) Nothing

-- | claims all possible ranks, asks specific card question,
-- | updates store
runTurn :: (Input m, Output m) => Player -> [Int] -> GameStore -> m ()
runTurn player sequence store = do
  let store' = claimAllPossibleRanks store player in
    runTurn player sequence store'
