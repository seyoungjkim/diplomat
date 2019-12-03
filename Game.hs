{-# OPTIONS -Wincomplete-patterns #-}
module Game where

import qualified State as S
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Map as Map
import Text.Read
import Question
import BuildQuestion
import GamePieces

-- Game represented as a GameStore and current player
type Game = S.State GameStore [Player]

data GameStore = G { players :: Map Int Player, faceUpCards :: [Card], currQuestion :: Question }

instance Show GameStore where
  show = undefined

-------------------------------------------------------------------------------

-- | initializes game given the number of total players and AIs
initialGameStore :: Int -> Int -> GameStore
initialGameStore n a =
  let n' = n - a
      pids = [0..n' - 1]
      aids = [n'..n - 1]
      hands = dealDeck n deck
      players = createPlayers pids (Prelude.take n' hands) False
      ais = createPlayers aids (Prelude.drop n' hands) True in
  G (Map.union players ais) [] Blank
  where
    createPlayers :: [Int] -> [PlayerHand] -> Bool -> Map Int Player
    createPlayers (id : ids) (h : hands) b = 
      Map.insert id (P id h Set.empty b) (createPlayers ids hands b)
    createPlayers _ _ _ = Map.empty
    createCycle :: [Player] -> [Player]
    createCycle ps = ps ++ createCycle ps

-- | checks if any player has won the game
checkEnd :: GameStore -> Bool
checkEnd gs = check [Ace ..] (players gs) where
  check :: [Rank] -> Map Int Player -> Bool
  check [] _ = True
  check (r:rs) players =
    Map.foldr (\p acc -> Set.member r (ranks p) || acc) False players && check rs players

-- | displays game statistics once the game ends
displayEnd :: Game -> IO ()
displayEnd = undefined

-- | make moves until someone wins
-- TODO: restrict arguments
main :: Int -> Int -> IO ()
main numPlayers numAI = let initialStore = initialGameStore numPlayers numAI
                            sequence = elems $ players initialStore in
  go sequence initialStore

go :: [Player] -> GameStore -> IO ()
go sequence store = 
  let (sequence', store') = S.runState (move sequence) store in do
    let player = sequence !! 0
    putStrLn ("Hi Player " ++ (show $ pid player) ++ "!! :)")
    putStrLn ("Laid out cards:")
    putStrLn (show $ faceUpCards store)
    putStrLn ("Your current claimed ranks:")
    putStrLn (show $ ranks player)
    putStrLn ("Your hand:")
    putStrLn (show $ hand player)
    putStrLn ("Do you want to claim any rank? Please enter y or n:")
    rankToClaim <- getLine
    if (rankToClaim == "y") then claimRankIO store player
    else putStrLn ("Ok, please ask a player a question now.")
    putStr ("Player " ++ (show $ pid player) ++ ": Enter a player id> ")
    playerIdToQuestion <- getLine
    case (readMaybe playerIdToQuestion :: Maybe Int) of
      Just i -> case Map.lookup i (players store) of
        Just playerToQuestion -> do
          putStrLn ("Player " ++ (show $ pid player) ++ ": Enter a question> ")
          putStrLn questionOptions
          q <- getLine
          case q of
            "1" -> questionSpecificCard playerToQuestion sequence store >> go sequence' store'
            "q" -> return () -- quit the game
            "none" -> go sequence' store' -- skip turn
            _   -> putStrLn "invalid question" >> go sequence store -- unknown command
        Nothing -> putStrLn "please enter a valid player id" >> go sequence store
      Nothing -> putStrLn "please enter an integer player id" >> go sequence store

questionSpecificCard :: Player -> [Player] -> GameStore -> IO ()
questionSpecificCard player sequence store = do
  putStrLn "Enter a rank."
  rank <- getLine
  putStrLn "Enter a suit: Diamond, Club, Heart, or Spade."
  suit <- getLine
  case (readMaybe suit :: Maybe Suit, readMaybe rank :: Maybe Rank) of
    (Just s, Just r) ->
      let c = (Card r s)
          q = SpecificCard c
          a =  getAnswer q (hand player) in
        putStrLn (show (q :: Question)) >>
        case a of
          True -> let gs' = layoutCard store player c in
                  putStrLn ("Yes, they had " ++ show c) >> go sequence gs'
          _ -> putStrLn ("No, they didn't have " ++ show c)
    _ -> putStrLn "invalid suit or rank" >> go sequence store

putBool :: Bool -> IO ()
putBool True = putStrLn "Yes"
putBool False = putStrLn "No"
          
questionOptions :: String  
questionOptions = "1: Specific card \n \
                  \2: Non-empty \n \
                  \3: Union \n \
                  \4: Intersection \n \
                  \5: Not \n \
                  \6: Equals \n \
                  \7: Greater than \n \
                  \8: Less than \n \
                  \9: Greater than or equal to \n \
                  \10: Greater than or equal to \n \
                  \done: finish the question \n \
                  \none: skip your turn"

questionIntOptions :: String  
questionIntOptions = "1: IntVal \n \
                     \2: Cardinality \n \
                     \3: SumHand \n \
                     \4: ProductHand \n \
                     \5: Sum \n \
                     \6: Diff \n \
                     \7: Mod \n \
                     \8: Product \n \
                     \done: finish the question \n \
                     \none: skip your turn"

-- TODO: how to have user input filter?
  -- I think they can create filters for each individual suit or rank for now?
  -- And then they can just combine them themselves?
questionHandOptions :: String
questionHandOptions = "1: Hand \n \
                      \2: Filter \n \
                      \3: UnionHand \n \
                      \4: IntersectionHand \n \
                      \done: finish the question \n \
                      \none: skip your turn"

claimRankIO :: GameStore -> Player -> IO ()
claimRankIO gs p = do
  putStrLn ("Please enter a valid rank:")
  rank <- getLine
  case (readMaybe rank :: Maybe Rank) of
    Just r -> 
      let gs' = claimRank gs p r in
      go (elems $ players gs) gs'
    _ -> putStrLn ("Please enter a valid rank:") >>
         claimRankIO gs p

-- THIS IS BROKEN!!!! not getting put into player ranks :(
-- need to add io message about claiming ranks
claimRank :: GameStore -> Player -> Rank -> GameStore
claimRank gs p r = let playerRanks = Prelude.filter (\c -> rank c == r) (Set.toList $ hand p)
                       laidOutRanks = Prelude.filter (\c -> rank c == r) (faceUpCards gs) in
  if (length playerRanks + length laidOutRanks == 4) then
    let newPlayerHand = Prelude.foldr (\c acc -> Set.delete c acc) (hand p) playerRanks
        newLaidOut = Prelude.foldr (\c acc -> List.delete c acc) (faceUpCards gs) laidOutRanks
        playerClaimedRanks = Set.insert r (ranks p)
        updatedPlayer = P (pid p) newPlayerHand playerClaimedRanks (ai p)
        newPlayerMap = Map.insert (pid p) updatedPlayer (players gs) in
    G newPlayerMap newLaidOut Blank       
  else gs   

-- layoutCardIO :: GameStore -> Player -> Card -> IO ()
-- layoutCardIO gs p c = undefined

layoutCard :: GameStore -> Player -> Card -> GameStore
layoutCard gs p c = let newPlayerHand = Set.delete c (hand p)
                        newFaceUp = c : (faceUpCards gs)
                        updatedPlayer = P (pid p) newPlayerHand (ranks p) (ai p)
                        newPlayerMap = Map.insert (pid p) updatedPlayer (players gs) in
  G newPlayerMap newFaceUp Blank

move :: [Player] -> Game
move [] = return []
move (x:xs) = do
  return (xs ++ [x])
