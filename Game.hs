{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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

data GameStore = G { players :: Map Int Player, laidOutCards :: [Card], currQuestion :: Question }

instance Show GameStore where
  show = undefined

class Monad m => Output m where
  write :: String -> m ()
class Monad m => Input m where
  input :: m (String)    -- only return input if it is ready
    
instance Output IO where
  write = putStrLn
instance Input IO where
  input = getLine

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
displayEnd :: (Input m, Output m) => Game -> m ()
displayEnd = undefined

instructions :: String
instructions = "Here are instructions! tbd..."

-- | make moves until someone wins
-- TODO: restrict arguments
main :: (Input m, Output m) => Int -> Int -> m ()
main numPlayers numAI = let initialStore = initialGameStore numPlayers numAI
                            sequence = [0..numPlayers + numAI] in do
  write "Welcome to Diplomat! \n"
  goIntro sequence initialStore

goIntro :: (Input m, Output m) => [Int] -> GameStore -> m ()
goIntro sequence store = 
  let player = (players store) ! (sequence !! 0) in do
    write $ "It's Player " ++ (show $ pid player) ++ "'s turn!!"
    write "Please make a move."
    playerCommand <- input
    case playerCommand of
      "help" -> write ("\n" ++ instructions ++ "\n") >>
                goIntro sequence store
      "quit" -> return ()
      "hand" -> write ("\nYour hand:") >>
                write (show (hand player) ++ "\n") >>
                goIntro sequence store
      "laidout" -> write ("\nCurrent laid out cards:") >>
                  write (show (laidOutCards store) ++ "\n") >>
                  goIntro sequence store
      "claimed" -> write ("\nYour current claimed ranks:") >>
                  write (show (Set.toList (ranks player)) ++ "\n") >>
                  goIntro sequence store
      "claim" -> goClaim sequence store
      "askq" -> goQuestion sequence store
      "all" -> write ("\nEntire game store:") >>
               write (show (players store) ++ "\n") >>
               goIntro sequence store
      _ -> write ("\nNot a valid menu command. Try again!\n") >>
           goIntro sequence store

goClaim :: (Input m, Output m) => [Int] -> GameStore -> m ()
goClaim sequence store =
  let player = (players store) ! (sequence !! 0) in do
    write "\nWhat rank do you want to claim?"
    toClaim <- input
    case toClaim of
      "quit" -> return ()
      "nvm" -> write ("\nNot claiming any ranks.") >>
               goIntro sequence store
      _ -> case (readMaybe toClaim :: Maybe Rank) of
             Just r -> let (store',b) = claimRank store player r in
                       if b then write ("\nCongrats, you successfully claimed the rank " ++ show r ++ " :)\n")
                       else write ("\nYou weren't able to claim the rank " ++ show r ++ " :(\n") >>
                       goIntro sequence store'
             _ -> write "\n Please enter a valid rank:" >>
                  goClaim sequence store

goQuestion :: (Input m, Output m) => [Int] -> GameStore -> m ()
goQuestion sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    let player = (players store) ! (sequence !! 0)
    write "\nOk, please choose a player to ask a question to."
    write ("Player " ++ (show $ pid player) ++ ": Enter a player id> ")
    playerIdToQuestion <- input
    case (readMaybe playerIdToQuestion :: Maybe Int) of
      Just i -> case Map.lookup i (players store) of
        Just playerToQuestion -> do
          write ("Player " ++ (show $ pid player) ++ ": Enter a question> ")
          write questionOptionsInitial
          q <- input
          case q of
            "1" -> askSpecificCard playerToQuestion sequence store
            "2" -> createQuestion playerToQuestion sequence store
            "quit" -> return () -- quit the game
            "none" -> goIntro sequence' store -- skip turn
            _   -> write "invalid question" >> goQuestion sequence store -- unknown command
        Nothing -> write "please enter a valid player id" >> goQuestion sequence store
      Nothing -> write "please enter an integer player id" >> goQuestion sequence store

askSpecificCard :: (Input m, Output m) => Player -> [Int] -> GameStore -> m ()
askSpecificCard player sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    write "Enter a rank."
    rank <- input
    write "Enter a suit: Diamond, Club, Heart, or Spade."
    suit <- input
    case (readMaybe suit :: Maybe Suit, readMaybe rank :: Maybe Rank) of
      (Just s, Just r) ->
        let c = (Card r s)
            q = SpecificCard c
            a =  getAnswer q (hand player) in
          write (show (q :: Question)) >>
          case a of
            True -> let gs' = layoutCard store player c in
                    write ("Yes, they had " ++ show c) >> goIntro sequence gs'
            _ -> write ("No, they didn't have " ++ show c) >> goIntro sequence' store
      _ -> write "invalid suit or rank" >> goQuestion sequence store

askComplexQuestion :: (Input m, Output m) => Player -> [Int] -> GameStore -> m ()
askComplexQuestion player sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    let ans = getAnswer (currQuestion store) (hand player)
    write ("You asked player " ++ (show $ pid player))
    write (show (currQuestion store))
    putBool ans
    goIntro sequence' store

putBool :: (Input m, Output m) => Bool -> m ()
putBool True = write "\nYes! :) \n"
putBool False = write "\nNo :(\n"

-- need to add io message about claiming ranks
claimRank :: GameStore -> Player -> Rank -> (GameStore, Bool)
claimRank gs p r = let playerRanks = Prelude.filter (\c -> rank c == r) (Set.toList $ hand p)
                       laidOutRanks = Prelude.filter (\c -> rank c == r) (laidOutCards gs) in
  if (length playerRanks + length laidOutRanks == 4) then
    let newPlayerHand = Prelude.foldr (\c acc -> Set.delete c acc) (hand p) playerRanks
        newLaidOut = Prelude.foldr (\c acc -> List.delete c acc) (laidOutCards gs) laidOutRanks
        playerClaimedRanks = Set.insert r (ranks p)
        updatedPlayer = P (pid p) newPlayerHand playerClaimedRanks (ai p)
        newPlayerMap = Map.insert (pid p) updatedPlayer (players gs) in
    (G newPlayerMap newLaidOut Blank, True)
  else (gs, False)

layoutCard :: GameStore -> Player -> Card -> GameStore
layoutCard gs p c = let newPlayerHand = Set.delete c (hand p)
                        newLaidOut = c : (laidOutCards gs)
                        updatedPlayer = P (pid p) newPlayerHand (ranks p) (ai p)
                        newPlayerMap = Map.insert (pid p) updatedPlayer (players gs) in
  G newPlayerMap newLaidOut Blank

createQuestion :: (Input m, Output m) => Player -> [Int] -> GameStore -> m ()
createQuestion player sequence gs = let currQ = currQuestion gs in 
  case findBlank currQ of
    1 -> createQuestionMain currQ
    2 -> createQuestionInt currQ
    3 -> createQuestionBool currQ
    _ -> askComplexQuestion player sequence gs
  where
    createQuestionMain :: (Input m, Output m) => Question -> m ()
    createQuestionMain currQ = do
      write (questionOptionsBuilding currQ)
      playerInput <- input
      case readQuestionOptionsBuilding playerInput of
        Nothing -> do
          write "Invalid input, try again!"
          createQuestionMain currQ
        Just q -> case buildQuestion currQ q of 
                    Nothing -> undefined -- should be unreachable
                    Just newQ -> createQuestion player sequence (gs {currQuestion = newQ})
    createQuestionInt :: (Input m, Output m) => Question -> m ()
    createQuestionInt currQ = do
      write (questionIntOptions currQ)
      playerInput <- input
      case readQuestionIntOptions playerInput of
        Nothing -> do 
          write "Invalid input, try again!"
          createQuestionInt currQ
        Just q -> case q of 
          IntVal _ -> do 
            write "Which integer?"
            i <- input
            case readMaybe i :: Maybe Int of
              Just i' -> let q = IntVal i' in
                case buildQuestionWithQInt currQ q of -- this code is duplicated below
                  Nothing -> undefined -- should be unreachable
                  Just newQ -> createQuestion player sequence (gs {currQuestion = newQ})
              Nothing -> do 
                write "Not an integer, try again!"
                createQuestionInt currQ
          _ -> case buildQuestionWithQInt currQ q of -- think about making the maybe as a helper function to get rid of the pattern match
                  Nothing -> undefined -- should be unreachable
                  Just newQ -> createQuestion player sequence (gs {currQuestion = newQ})
    createQuestionBool :: (Input m, Output m) => Question -> m ()
    createQuestionBool currQ = do 
      write (questionHandOptions currQ)
      playerInput <- input
      case readQuestionHandOptions playerInput of
        Nothing -> do 
          write "Invalid input, try again!"
          createQuestionInt currQ
        Just q -> case buildQuestionWithQHand currQ q of 
          Nothing -> undefined -- should be unreachable
          Just newQ -> createQuestion player sequence (gs {currQuestion = newQ})

move :: [Int] -> Game
move [] = return []
move (x:xs) = do
  return (xs ++ [x])
