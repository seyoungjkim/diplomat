{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE LambdaCase #-}

module GameState where

import State as S (State)
import Data.Maybe (Maybe)
import Data.Set (Set)
import qualified Data.Set as Set (member, toList, fromList, delete, size,
                                 empty, insert)
import qualified Data.List as List (foldr, filter, dropWhile, drop, delete)
import Data.Map (Map)
import qualified Data.Map as Map (empty, union, insert, lookup, foldr, keys)

import Question (Question (Blank))
import GamePieces (Rank (Ace), Card, Player (P), PlayerHand, CardGuess, rank,
                   pid, hand, ranks, ai, aiGuess, deck, dealDeck)
import GameTexts (showBool)

-------------------------------------------------------------------------------
-- Represents a player's action
data Move = 
  MQuestion Int Question Int Bool -- Player id1 asked Player id2
  | MLayout Int Card              -- Player id laid out card
  | MClaim Int Rank               -- Player id claimed rank
  | MBreak Int                    -- Player id ended their turn

instance Show Move where
  show (MBreak i) = "Player " ++ show i ++ " ended their turn!"
  show (MClaim i r) = "Player " ++ show i ++ " claimed the " ++ 
                      show r ++ "s!"
  show (MLayout i c) = "Player " ++ show i ++ " laid out the " ++ 
                       show c ++ "."
  show (MQuestion i1 q i2 b) = "Player " ++ show i1 ++ " asked Player " ++ 
                               show i2 ++ " <" ++ show q ++ 
                               "> and they answered " ++ showBool b

-- Represents the current Game state and the current sequence of players
type Game = S.State GameStore [Int]

-- Key components stored in game
data GameStore = G { players :: Map Int Player, 
                     laidOutCards :: [Card], 
                     currQuestion :: Question,
                     prevMoves :: [Move] }    
  deriving (Show)   

-------------------------------------------------------------------------------
-- | Initializes game given the number of human players and AI players
initialGameStore :: Int -> Int -> Int -> GameStore
initialGameStore seed n a =
  let total = n + a
      pids = [0..n - 1]
      aids = [n..total - 1]
      allIds = pids ++ aids
      hands = dealDeck seed total deck
      players = createPlayers pids (Prelude.take n hands) False allIds
      ais = createPlayers aids (Prelude.drop n hands) True allIds
      moveBreaks = map MBreak allIds in
  G (Map.union players ais) [] Blank moveBreaks
  where
    createPlayers :: [Int] -> [PlayerHand] -> Bool -> [Int] -> Map Int Player
    createPlayers (id : ids) (h : hands) b allIds = 
      Map.insert id (P id h Set.empty b cg) (createPlayers ids hands b allIds)
      where cg = if b then initialAiGuess id h allIds else Map.empty
    createPlayers _ _ _ _ = Map.empty

-- | Creates initial map of all possible card-player assignments for AI players
initialAiGuess :: Int -> PlayerHand -> [Int] -> CardGuess
initialAiGuess aid aiHand playerIds = List.foldr 
  (\c m -> Map.insert c (Set.fromList filteredIds) m) Map.empty filteredDeck
  where filteredDeck = List.filter (\c -> not $ Set.member c aiHand) deck
        filteredIds = List.filter (/= aid) playerIds

-- | Checks if all ranks have been claimed and if game has ended.
checkEnd :: GameStore -> Bool
checkEnd store = check [Ace ..] (players store) where
  check :: [Rank] -> Map Int Player -> Bool
  check [] _ = True
  check (r:rs) players =
    Map.foldr (\p acc -> Set.member r (ranks p) || acc) False players && 
    check rs players

-- | Returns player with the most ranks claimed    
getWinner :: GameStore -> Player
getWinner store = 
    let sequence = Map.keys (players store) in 
  Map.foldr 
    (\p acc -> if Set.size (ranks p) >= Set.size (ranks acc) then p else acc)
  (P (-1) Set.empty Set.empty False Map.empty) (players store)

-- | String representation of the ranks each player has claimed. 
getPlayerRanksString :: GameStore -> String
getPlayerRanksString store = Map.foldr 
  (\p acc -> let n = Set.size (ranks p) in
    "\nPlayer " ++ show (pid p) ++ " collected " ++ show n ++ " ranks:" 
    ++ prettyPrintList (Set.toList (ranks p)) " :(" ++ "\n" ++ acc)
  "" (players store)

-- | Updates GameStore and adds the claimed rank to the specified player.
-- Returns nothing if rank could not be claimed.
claimRank :: GameStore -> Int -> Rank -> Maybe GameStore
claimRank store playerId r =
  case Map.lookup playerId (players store) of
    Nothing -> Nothing
    Just player -> 
      let playerRanks = Prelude.filter (\c -> rank c == r) 
                          (Set.toList $ hand player)
          laidOutRanks = Prelude.filter (\c -> rank c == r) 
                          (laidOutCards store) in
      if length playerRanks + length laidOutRanks == 4 then
        let newPlayerHand = 
              Prelude.foldr Set.delete (hand player) playerRanks
            newLaidOut = 
              Prelude.foldr List.delete (laidOutCards store) laidOutRanks
            playerClaimedRanks = Set.insert r (ranks player)
            updatedPlayer = player { hand = newPlayerHand, 
                                    ranks = playerClaimedRanks }
            newPlayerMap = Map.insert playerId updatedPlayer (players store)
            newPrevMoves = prevMoves store ++ [MClaim playerId r]
            in
        Just $ G newPlayerMap newLaidOut Blank newPrevMoves
      else Nothing

-- | Updates GameStore and lays out the card from the specified player.
-- Returns the updated GameStore.
layoutCard :: GameStore -> Player -> Card -> GameStore
layoutCard store p c = 
  let playerId = pid p
      newPlayerHand = Set.delete c (hand p)
      newLaidOut = c : laidOutCards store
      updatedPlayer = P playerId newPlayerHand (ranks p) (ai p) (aiGuess p)
      newPlayerMap = Map.insert playerId updatedPlayer (players store)
      newPrevMoves = prevMoves store ++ [MLayout playerId c]
      in
  G newPlayerMap newLaidOut Blank newPrevMoves

-- | Updates player sequence of game to the next player's move
move :: [Int] -> Game
move [] = return []
move (x:xs) = return (xs ++ [x])

-- | Helper function to check if given Move is a player break
isPlayerBreak :: Int -> Move -> Bool
isPlayerBreak id m = case m of
  (MBreak i) -> i == id
  _ -> False

-- | Helper function to check if list of moves container a player break
hasPlayerBreak :: Int -> [Move] -> Bool
hasPlayerBreak id = List.foldr (\m acc -> isPlayerBreak id m || acc) False

-- | Removes a player's actions from their previous turn in the list of moves,
-- which are all the actions before their last player break
clearPrevMoves :: Int -> [Move] -> [Move]
clearPrevMoves id ms = if hasPlayerBreak id ms
  then List.drop 1 $ List.dropWhile (not . isPlayerBreak id) ms
  else ms

-- | Filters out all player breaks in the list of moves
filterMoveBreaks :: [Move] -> [Move]
filterMoveBreaks = List.filter 
  (\case
      (MBreak _) -> False
      _          -> True
  )

-- | Prints a list of elements in bulleted points
prettyPrintList :: (Show a) => [a] -> String -> String
prettyPrintList [] s = s
prettyPrintList l _ = List.foldr (\x acc -> "\n-" ++ show x ++ acc) "" l
