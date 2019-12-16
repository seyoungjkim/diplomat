module GameState where

import qualified State as S
import qualified Data.Maybe as Maybe
import Data.Set as Set
import qualified Data.List as List
import Data.Map as Map
import Text.Read
import Question
import BuildQuestion
import GamePieces

-- Game represented as a GameStore and current player
data Move = 
  MQuestion Int Question Int Bool -- i1 asked i2
  | MLayout Int Card
  | MClaim Int Rank
  | MBreak Int

instance Show Move where
  show (MBreak i) = "Player " ++ (show i) ++ " ended their turn!"
  show (MClaim i r) = "Player " ++ (show i) ++ " claimed the " ++ (show r) ++ "s!"
  show (MLayout i c) = "Player " ++ (show i) ++ " laid out the " ++ (show c) ++ "."
  show (MQuestion i1 q i2 b) = "Player " ++ (show i1) ++ " asked Player " ++ (show i2) ++ 
    " <" ++ (show q) ++ "> and they answered " ++ (showBool b)

type Game = S.State GameStore [Int]

data GameStore = G { players :: Map Int Player, 
                     laidOutCards :: [Card], 
                     currQuestion :: Question,
                     prevMoves :: [Move] }    
  deriving (Show)   

showBool :: Bool -> String
showBool True = "Yes! :)"
showBool False = "No :("

-- | initial map of all possible card assignments
initialAiGuess :: PlayerHand -> [Int] -> CardGuess
initialAiGuess aiHand playerIds = List.foldr 
  (\c m -> Map.insert c (Set.fromList playerIds) m) Map.empty filteredDeck
  where filteredDeck = List.foldr (\c acc -> 
                         if Set.member c aiHand then acc else c : acc) [] deck

-- | initializes game given the number of non-AI players and AIs
initialGameStore :: Int -> Int -> GameStore
initialGameStore n a =
  let total = n + a
      pids = [0..n - 1]
      aids = [n..total]
      allIds = pids ++ aids
      hands = dealDeck total deck
      players = createPlayers pids (Prelude.take n hands) False allIds
      ais = createPlayers aids (Prelude.drop n hands) True allIds
      moveBreaks = List.foldr (\i acc -> (MBreak i) : acc) [] allIds in
  G (Map.union players ais) [] Blank moveBreaks
  where
    createPlayers :: [Int] -> [PlayerHand] -> Bool -> [Int] -> Map Int Player
    createPlayers (id : ids) (h : hands) b allIds = 
      Map.insert id (P id h Set.empty b cg) (createPlayers ids hands b allIds)
      where cg = if b then (initialAiGuess h allIds) else Map.empty
    createPlayers _ _ _ _ = Map.empty
      

-- | returns nothing if claim was not successful
claimRank :: GameStore -> Player -> Rank -> Maybe GameStore
claimRank store p r = 
  let playerRanks = Prelude.filter (\c -> rank c == r) (Set.toList $ hand p)
      laidOutRanks = Prelude.filter (\c -> rank c == r) (laidOutCards store) in
  if length playerRanks + length laidOutRanks == 4 then
    let playerId = pid p
        newPlayerHand = 
          Prelude.foldr Set.delete (hand p) playerRanks
        newLaidOut = 
          Prelude.foldr List.delete (laidOutCards store) laidOutRanks
        playerClaimedRanks = Set.insert r (ranks p)
        updatedPlayer = P playerId newPlayerHand playerClaimedRanks (ai p) (aiGuess p)
        newPlayerMap = Map.insert playerId updatedPlayer (players store)
        newPrevMoves = (prevMoves store) ++ [(MClaim playerId r)]
        in
    Just $ G newPlayerMap newLaidOut Blank newPrevMoves
  else Nothing

-- | checks if any player has won the game
checkEnd :: GameStore -> Bool
checkEnd store = check [Ace ..] (players store) where
  check :: [Rank] -> Map Int Player -> Bool
  check [] _ = True
  check (r:rs) players =
    Map.foldr (\p acc -> Set.member r (ranks p) || acc) False players && 
    check rs players

getWinner :: GameStore -> Player
getWinner store = 
    let sequence = Map.keys (players store) in 
  Map.foldr 
    (\p acc -> if Set.size (ranks p) > Set.size (ranks acc) then p else acc)
  (players store ! head sequence) (players store)

getPlayerRanksString :: GameStore -> String
getPlayerRanksString store = Map.foldr 
  (\p acc -> let n = Set.size (ranks p) in
    "\nPlayer " ++ show (pid p) ++ " collected " ++ show n ++ " ranks:" 
    ++ (prettyPrintList (Set.toList (ranks p)) " :(") ++ "\n" ++ acc)
  "" (players store)

layoutCard :: GameStore -> Player -> Card -> GameStore
layoutCard store p c = 
  let playerId = pid p
      newPlayerHand = Set.delete c (hand p)
      newLaidOut = c : laidOutCards store
      updatedPlayer = P playerId newPlayerHand (ranks p) (ai p) (aiGuess p)
      newPlayerMap = Map.insert playerId updatedPlayer (players store)
      newPrevMoves = (prevMoves store) ++ [(MLayout playerId c)]
      in
  G newPlayerMap newLaidOut Blank newPrevMoves

move :: [Int] -> Game
move [] = return []
move (x:xs) = return (xs ++ [x])

isPlayerBreak :: Int -> Move -> Bool
isPlayerBreak id m = case m of
  (MBreak i) -> i == id
  _ -> False

hasPlayerBreak :: Int -> [Move] -> Bool
hasPlayerBreak id = List.foldr (\m acc -> isPlayerBreak id m || acc) False

clearPrevMoves :: Int -> [Move] -> [Move]
clearPrevMoves id ms = if hasPlayerBreak id ms
  then List.drop 1 $ List.dropWhile (\m -> not $ isPlayerBreak id m) ms
  else ms

filterMoveBreaks :: [Move] -> [Move]
filterMoveBreaks = List.filter 
  (\m -> case m of
    (MBreak _) -> False
    _          -> True
  )

prettyPrintList :: (Show a) => [a] -> String -> [Char]
prettyPrintList [] s = s
prettyPrintList l _ = List.foldr (\x acc -> "\n-" ++ (show x) ++ acc) "" l