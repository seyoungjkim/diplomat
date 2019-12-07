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
type Game = S.State GameStore [Int]

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
                            sequence = [0..numPlayers + numAI] in
  goIntro sequence initialStore

goIntro :: [Int] -> GameStore -> IO ()
goIntro sequence store = 
  let player = (players store) ! (sequence !! 0) in do
    putStrLn ("\nHi Player " ++ (show $ pid player) ++ "!! :)")
    putStrLn ("Curr claimed ranks:")
    putStrLn (show $ (players store))
    putStrLn ("Laid out cards:")
    putStrLn (show $ faceUpCards store)
    putStrLn ("Your current claimed ranks:")
    putStrLn (show $ Set.toList (ranks player))
    putStrLn ("Your hand:")
    putStrLn (show $ hand player)
    goClaim sequence store

goClaim :: [Int] -> GameStore -> IO ()
goClaim sequence store =
  let player = (players store) ! (sequence !! 0) in do
    claimRankIO store player sequence
    goQuestion sequence store

goQuestion :: [Int] -> GameStore -> IO ()
goQuestion sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    let player = (players store) ! (sequence !! 0)
    putStrLn ("Ok, please ask a player a question now.")
    putStr ("Player " ++ (show $ pid player) ++ ": Enter a player id> ")
    playerIdToQuestion <- getLine
    case (readMaybe playerIdToQuestion :: Maybe Int) of
      Just i -> case Map.lookup i (players store) of
        Just playerToQuestion -> do
          putStrLn ("Player " ++ (show $ pid player) ++ ": Enter a question> ")
          putStrLn questionOptionsInitial
          q <- getLine
          case q of
            "1" -> questionSpecificCard playerToQuestion sequence store
            "q" -> return () -- quit the game
            "none" -> goIntro sequence' store -- skip turn
            _   -> putStrLn "invalid question" >> goQuestion sequence store -- unknown command
        Nothing -> putStrLn "please enter a valid player id" >> goQuestion sequence store
      Nothing -> putStrLn "please enter an integer player id" >> goQuestion sequence store

questionSpecificCard :: Player -> [Int] -> GameStore -> IO ()
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
                  putStrLn ("Yes, they had " ++ show c) >> goClaim sequence gs'
          _ -> putStrLn ("No, they didn't have " ++ show c)
    _ -> putStrLn "invalid suit or rank" >> goQuestion sequence store

putBool :: Bool -> IO ()
putBool True = putStrLn "Yes"
putBool False = putStrLn "No"

claimRankIO :: GameStore -> Player -> [Int] -> IO ()
claimRankIO gs p sequence = do
  putStrLn "Do you want to claim a rank?"
  claimYN <- getLine
  if (claimYN == "y") then do
    putStrLn "Please enter a valid rank:"
    rank <- getLine
    case (readMaybe rank :: Maybe Rank) of
      Just r -> 
        let gs' = claimRank gs p r in
        goClaim sequence gs'
      _ -> putStrLn "Please enter a valid rank:" >>
          claimRankIO gs p sequence
  else putStr "" -- this is dumb

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

createQuestion :: GameStore -> IO ()
createQuestion gs = let currQ = currQuestion gs in 
  case findBlank currQ of
    1 -> createQuestionMain currQ
    2 -> createQuestionInt currQ
    3 -> createQuestionBool currQ
    _ -> putStr "" -- go gs, what are we doing with the question now that it's made?
  where createQuestionMain :: Question -> IO ()
        createQuestionMain currQ = 
          do putStrLn (questionOptionsBuilding currQ)
             input <- getLine
             case readQuestionOptionsBuilding input of
               Nothing -> do putStrLn "Invalid input, try again!"
                             createQuestionMain currQ
               Just q -> case buildQuestion currQ q of 
                 Nothing -> undefined -- should be unreachable
                 Just newQ -> createQuestion (gs {currQuestion = newQ})
        createQuestionInt currQ = 
          do putStrLn (questionIntOptions currQ)
             input <- getLine
             case readQuestionIntOptions input of
               Nothing -> do putStrLn "Invalid input, try again!"
                             createQuestionInt currQ
               Just q -> case q of 
                IntVal _ -> do putStrLn "Which integer?"
                               i <- getLine
                               case readMaybe i :: Maybe Int of
                                 Just i' -> let q = IntVal i' in
                                  case buildQuestionWithQInt currQ q of -- this code is duplicated below
                                    Nothing -> undefined -- should be unreachable
                                    Just newQ -> createQuestion (gs {currQuestion = newQ})
                                 Nothing -> do putStrLn "Not an integer!"
                                               createQuestionInt currQ
                _ -> case buildQuestionWithQInt currQ q of -- think about making the maybe as a helper function to get rid of the pattern match
                       Nothing -> undefined -- should be unreachable
                       Just newQ -> createQuestion (gs {currQuestion = newQ})
        createQuestionBool currQ = 
          do putStrLn (questionHandOptions currQ)
             input <- getLine
             case readQuestionHandOptions input of
               Nothing -> do putStrLn "Invalid input, try again!"
                             createQuestionInt currQ
               Just q -> case buildQuestionWithQHand currQ q of 
                 Nothing -> undefined -- should be unreachable
                 Just newQ -> createQuestion (gs {currQuestion = newQ})

move :: [Int] -> Game
move [] = return []
move (x:xs) = do
  return (xs ++ [x])
