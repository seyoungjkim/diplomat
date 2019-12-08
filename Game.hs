{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
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

data GameStore = G { players :: Map Int Player, 
                     laidOutCards :: [Card], 
                     currQuestion :: Question }

instance Show GameStore where
  show = undefined

class Monad m => Output m where
  write :: String -> m ()
class Monad m => Input m where
  input :: m String    -- only return input if it is ready
    
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
    Map.foldr (\p acc -> Set.member r (ranks p) || acc) False players && 
    check rs players

-- | displays game statistics once the game ends
displayEnd :: (Input m, Output m) => Game -> m ()
displayEnd = undefined

instructions :: String
instructions = "=== INSTRUCTIONS ===\n\
               \Diplomat is very similar to the popular card game, Go Fish.\n\
               \Cards are distributed evenly among all players, and then \n\
               \players take turns questioning each other. \n\n\
               \The goal of the game is to collect as many ranks as possible,\n\
               \where claiming a rank means collecting all four cards of the\n\ 
               \rank (i.e. you can claim the Ace rank when you've located all\n\
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
               \are either in your hand or laid out)."

commands :: String
commands = "Here are commands you can use:\n \
           \help: to view what commands you can use\n \
           \instr: to view Diplomat instructions\n \
           \hand: to view the cards in your hand\n \
           \laidout: to view currently laid-out cards\n \
           \claimed: to view the ranks you have claimed\n \
           \claim: to claim a rank\n \
           \ask: to ask another player a question\n \
           \quit: to quit the game.\n"

-- | make moves until someone wins
-- TODO: restrict arguments
play :: (Input m, Output m) => Int -> Int -> m ()
play numPlayers numAI = let initialStore = initialGameStore numPlayers numAI
                            sequence = [0..numPlayers + numAI] in do
  write "Welcome to Diplomat!\n"
  write commands
  goIntro sequence initialStore

goIntro :: (Input m, Output m) => [Int] -> GameStore -> m ()
goIntro sequence store = 
  let player = players store ! head sequence in do
    write $ "It's Player " ++ show (pid player) ++ "'s turn!!"
    write "Please type a command."
    playerCommand <- input
    case playerCommand of
      "help" -> write ("\n" ++ commands ++ "\n") >>
                goIntro sequence store
      "instr" -> write ("\n" ++ instructions ++ "\n") >>
                 goIntro sequence store
      "quit" -> return ()
      "hand" -> write "\nYour hand:" >>
                write (show (hand player) ++ "\n") >>
                goIntro sequence store
      "laidout" -> write "\nCurrent laid out cards:" >>
                   write (show (laidOutCards store) ++ "\n") >>
                   goIntro sequence store
      "claimed" -> write "\nYour current claimed ranks:" >>
                  write (show (Set.toList (ranks player)) ++ "\n") >>
                  goIntro sequence store
      "claim" -> goClaim sequence store
      "ask" -> goQuestion sequence store
      "all" -> write "\nEntire game store:" >>
               write (show (players store) ++ "\n") >>
               goIntro sequence store
      _ -> write "\nNot a valid menu command. Try again!\n" >>
           goIntro sequence store

goClaim :: (Input m, Output m) => [Int] -> GameStore -> m ()
goClaim sequence store =
  let player = players store ! head sequence in do
    write "\nWhat rank do you want to claim?"
    toClaim <- input
    case toClaim of
      "quit" -> return ()
      "nvm" -> write "\nNot claiming any ranks." >>
               goIntro sequence store
      _ -> case (readMaybe toClaim :: Maybe Rank) of
             Just r -> let (store',b) = claimRank store player r in
                       if b then 
                        write ("\nCongrats, you successfully claimed the rank " 
                          ++ show r ++ " :)\n")
                       else write ("\nYou weren't able to claim the rank " 
                        ++ show r ++ " :(\n") >>
                       goIntro sequence store'
             _ -> write "\n Please enter a valid rank:" >>
                  goClaim sequence store

goQuestion :: (Input m, Output m) => [Int] -> GameStore -> m ()
goQuestion sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    let player = players store ! head sequence
    write "\nOk, please choose a player to ask a question to."
    write ("Player " ++ show (pid player) ++ ": Enter a player id> ")
    playerIdToQuestion <- input
    case (readMaybe playerIdToQuestion :: Maybe Int) of
      Just i -> case Map.lookup i (players store) of
        Just playerToQuestion -> do
          write ("Player " ++ show (pid player) ++ ": Enter a question> ")
          write questionOptionsInitial
          q <- input
          case q of
            "1" -> askSpecificCard playerToQuestion sequence store
            "2" -> createQuestion playerToQuestion sequence store
            "quit" -> return () -- quit the game
            "none" -> goIntro sequence' store -- skip turn
            _   -> write "invalid question" >> goQuestion sequence store
        Nothing -> write "please enter a valid player id" 
                   >> goQuestion sequence store
      Nothing -> write "please enter an integer player id" 
                 >> goQuestion sequence store

askSpecificCard :: (Input m, Output m) => Player -> [Int] -> GameStore -> m ()
askSpecificCard player sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    write "Enter a rank."
    rank <- input
    write "Enter a suit: Diamond, Club, Heart, or Spade."
    suit <- input
    case (readMaybe suit :: Maybe Suit, readMaybe rank :: Maybe Rank) of
      (Just s, Just r) ->
        let c = Card r s
            q = SpecificCard c
            a =  getAnswer q (hand player) in
          write (show (q :: Question)) >>
          if a 
            then let gs' = layoutCard store player c in
                 write ("Yes, they had " ++ show c) >> goIntro sequence gs'
            else write ("No, they didn't have " ++ show c) 
                 >> goIntro sequence' store
      _ -> write "invalid suit or rank" >> goQuestion sequence store

askComplexQuestion :: (Input m, Output m) => 
  Player -> [Int] -> GameStore -> m ()
askComplexQuestion player sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    let ans = getAnswer (currQuestion store) (hand player)
    write ("You asked Player " ++ show (pid player) ++ ":")
    write (show (currQuestion store))
    putBool ans
    goIntro sequence' store

putBool :: (Input m, Output m) => Bool -> m ()
putBool True = write "\nYes! :) \n"
putBool False = write "\nNo :(\n"

-- need to add io message about claiming ranks
claimRank :: GameStore -> Player -> Rank -> (GameStore, Bool)
claimRank gs p r = 
  let playerRanks = Prelude.filter (\c -> rank c == r) (Set.toList $ hand p)
      laidOutRanks = Prelude.filter (\c -> rank c == r) (laidOutCards gs) in
  if length playerRanks + length laidOutRanks == 4 then
    let newPlayerHand = 
          Prelude.foldr Set.delete (hand p) playerRanks
        newLaidOut = 
          Prelude.foldr List.delete (laidOutCards gs) 
          laidOutRanks
        playerClaimedRanks = Set.insert r (ranks p)
        updatedPlayer = P (pid p) newPlayerHand playerClaimedRanks (ai p)
        newPlayerMap = Map.insert (pid p) updatedPlayer (players gs) in
    (G newPlayerMap newLaidOut Blank, True)
  else (gs, False)

layoutCard :: GameStore -> Player -> Card -> GameStore
layoutCard gs p c = 
  let newPlayerHand = Set.delete c (hand p)
      newLaidOut = c : laidOutCards gs
      updatedPlayer = P (pid p) newPlayerHand (ranks p) (ai p)
      newPlayerMap = Map.insert (pid p) updatedPlayer (players gs) in
  G newPlayerMap newLaidOut Blank

createQuestion :: (Input m, Output m) => Player -> [Int] -> GameStore -> m ()
createQuestion player sequence gs = let currQ = currQuestion gs in 
  case findBlank currQ of
    1 -> createQuestionMain currQ
    2 -> createQuestionInt currQ
    3 -> createQuestionHand currQ
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
        Just q -> 
          case buildQuestion currQ q of 
            Nothing -> undefined -- should be unreachable
            Just newQ -> 
              createQuestion player sequence (gs {currQuestion = newQ})
    createQuestionInt :: (Input m, Output m) => Question -> m ()
    createQuestionInt currQ = do
      write (questionIntOptions currQ)
      playerInput <- input
      case readQuestionIntOptions playerInput of
        Nothing -> do 
          write "Invalid input, try again!"
          createQuestionInt currQ
        Just (IntVal _) -> do 
          write "Which integer?"
          i <- input
          case readMaybe i :: Maybe Int of
            Just i' -> let q = IntVal i' in
              case buildQuestionWithQInt currQ q of 
                Nothing -> undefined -- should be unreachable
                Just newQ -> 
                  createQuestion player sequence (gs {currQuestion = newQ})
            Nothing -> do 
              write "Not an integer, try again!"
              createQuestionInt currQ
        Just q -> case buildQuestionWithQInt currQ q of
                Nothing -> undefined -- should be unreachable
                Just newQ -> 
                  createQuestion player sequence (gs {currQuestion = newQ})
    createQuestionHand :: (Input m, Output m) => Question -> m ()
    createQuestionHand currQ = do 
      write (questionHandOptions currQ)
      playerInput <- input
      case readQuestionHandOptions playerInput of
        Nothing -> do 
          write "Invalid input, try again!"
          createQuestionInt currQ
        Just (Filter _ qh) -> createQuestionFilter currQ Set.empty Set.empty
        Just q -> case buildQuestionWithQHand currQ q of 
          Nothing -> undefined -- should be unreachable
          Just newQ -> createQuestion player sequence (gs {currQuestion = newQ})
    createQuestionFilter :: (Input m, Output m) => 
      Question -> Set.Set Suit -> Set.Set Rank -> m ()
    createQuestionFilter currQ filteredSuits filteredRanks = do
      write "\nSo far, you've filtered out:"
      write (show filteredSuits)
      write (show filteredRanks)
      write $ "What would you like to filter out? Enter any rank, suit, or" ++ 
              " 'done' if there's nothing else to filter."
      filterOut <- input
      case (readMaybe filterOut :: Maybe Suit) of
        Just s -> 
          createQuestionFilter currQ (Set.insert s filteredSuits) filteredRanks
        Nothing -> case (readMaybe filterOut :: Maybe Rank) of
          Just r -> 
            createQuestionFilter currQ filteredSuits 
              (Set.insert r filteredRanks)
          Nothing -> case filterOut of
            "done" -> 
              let filterFunction c = not (suit c `Set.member` filteredSuits) && 
                                     not (rank c `Set.member` filteredRanks)
                  filterQuestion = Filter filterFunction BlankQHand in
              case buildQuestionWithQHand currQ filterQuestion of 
                Nothing -> undefined -- should be unreachable
                Just newQ -> createQuestion player sequence 
                             (gs {currQuestion = newQ})
            _ -> write "Invalid input, try again!" >>
                 createQuestionFilter currQ filteredSuits filteredRanks

move :: [Int] -> Game
move [] = return []
move (x:xs) = return (xs ++ [x])
