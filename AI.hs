{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module AI where
  
import qualified Data.Set as Set (delete, lookupMin, null, lookupMin)
import qualified Data.Map as Map (filterWithKey, insert, lookup, foldrWithKey, update)

import GamePieces (Rank (Ace), Card, CardGuess, pid, hand, aiGuess)
import GameState (Move (MQuestion), GameStore, players, laidOutCards, 
                prevMoves, claimRank, layoutCard)
import Question (Question (SpecificCard), getAnswer)

-------------------------------------------------------------------------------
-- | removes a player id from possible card guesses. if no players remain to be
-- guessed, then card is removed from the map of possible guesses.
updateCardGuess :: CardGuess -> Card -> Int -> CardGuess
updateCardGuess cg card pid =
  let f x = if Set.null x' then Nothing else Just x' 
            where x' = Set.delete pid x in
  Map.update f card cg

-- | attempts to claim all possible ranks
claimAllPossibleRanks :: GameStore -> Int -> GameStore
claimAllPossibleRanks store playerId = claim store playerId [Ace ..] where
  claim :: GameStore -> Int -> [Rank] -> GameStore
  claim store playerId [] = store
  claim store playerId (r:rs) = case claimRank store playerId r of
    Nothing -> claim store playerId rs
    Just store' -> claim store' playerId rs

-- | choose a card and player to ask about (first pair found in map)
getNextCard :: CardGuess -> Maybe (Card, Int)
getNextCard = Map.foldrWithKey 
  (\k x _ -> Set.lookupMin x >>= (\x' -> Just (k, x'))) Nothing

-- | removes laid out cards from guess
filterKnownCards :: [Card] -> CardGuess -> CardGuess
filterKnownCards laidOutCards =
  Map.filterWithKey (\c _ -> c `notElem` laidOutCards)

-- | claims all possible ranks, asks specific card question,
-- then updates the game store
runAiTurn :: GameStore -> Int -> GameStore
runAiTurn store playerId =
  let store' = claimAllPossibleRanks store playerId
      allPlayers = players store' in
    case Map.lookup playerId (players store') of
      Just askingPlayer -> 
        let guess = filterKnownCards (laidOutCards store') 
                    (aiGuess askingPlayer) in
          case getNextCard guess of
            Just (card, askedPlayerId) ->
              case Map.lookup askedPlayerId allPlayers of
                Just askedPlayer ->
                  let question = SpecificCard card
                      answer = getAnswer question (hand askedPlayer)
                      guess' = updateCardGuess guess card askedPlayerId
                      updatedAskingPlayer = askingPlayer { aiGuess = guess' }
                      updatedPlayerMap = Map.insert playerId
                                          updatedAskingPlayer allPlayers
                      oldPrevMoves = prevMoves store'
                      updatedPrevMoves = oldPrevMoves ++ [ MQuestion 
                        (pid askingPlayer) question (pid askedPlayer) answer ]
                      store'' = store' { players = updatedPlayerMap,
                                        prevMoves = updatedPrevMoves } in
                  if answer then 
                    let store''' = layoutCard store'' askedPlayer card in
                    runAiTurn store''' playerId
                  else store'' { prevMoves = updatedPrevMoves }
                Nothing -> store'
            Nothing -> store'
      Nothing -> store'
