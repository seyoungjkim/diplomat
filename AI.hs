{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module AI where
import GamePieces
import GameState
import Question
import Data.Set as Set
import Data.Map as Map

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
getNextCard :: CardGuess -> Maybe (Card, Int)
getNextCard = Map.foldrWithKey 
  (\k x _ -> Set.lookupMin x >>= (\x' -> Just (k, x'))) Nothing

-- | removes laid out cards from guess
filterKnownCards :: [Card] -> CardGuess -> CardGuess
filterKnownCards laidOutCards =
  Map.filterWithKey (\c _ -> not (elem c laidOutCards))

-- | claims all possible ranks, asks specific card question,
-- | updates store
runAiTurn :: GameStore -> Player -> GameStore
runAiTurn store askingPlayer =
  let store' = claimAllPossibleRanks store askingPlayer
      guess = filterKnownCards (laidOutCards store') (aiGuess askingPlayer) in
    case getNextCard guess of
      Just (card, askedPlayerId) ->
        case Map.lookup askedPlayerId (players store') of
          Just askedPlayer ->
            let question = SpecificCard card
                answer = getAnswer question (hand askedPlayer)
                guess' = updateCardGuess guess card askedPlayerId
                updatedAskingPlayer = askingPlayer { aiGuess = guess' }
                updatedPlayerMap = Map.insert (pid askingPlayer) 
                                     updatedAskingPlayer (players store)
                store'' = store' { players = updatedPlayerMap,
                                   prevMoves = (prevMoves store) ++ 
                                   [ (MQuestion (pid askingPlayer) 
                                   question (pid askedPlayer) answer) ] } in
            if answer then runAiTurn store'' askingPlayer 
            else store'' { prevMoves = (prevMoves store'') ++ 
                         [ (MBreak (pid askingPlayer)) ] }
          Nothing -> store'
      Nothing -> store'
