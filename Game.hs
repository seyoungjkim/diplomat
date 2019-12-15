{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
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
      ais = createPlayers aids (Prelude.drop n' hands) True 
      moveBreaks = List.foldr (\i acc -> (MBreak i) : acc) [] (pids ++ aids) in
  G (Map.union players ais) [] Blank moveBreaks
  where
    createPlayers :: [Int] -> [PlayerHand] -> Bool -> Map Int Player
    createPlayers (id : ids) (h : hands) b = 
      Map.insert id (P id h Set.empty b) (createPlayers ids hands b)
    createPlayers _ _ _ = Map.empty

-- | checks if any player has won the game
checkEnd :: GameStore -> Bool
checkEnd store = check [Ace ..] (players store) where
  check :: [Rank] -> Map Int Player -> Bool
  check [] _ = True
  check (r:rs) players =
    Map.foldr (\p acc -> Set.member r (ranks p) || acc) False players && 
    check rs players

-- | displays game statistics once the game ends
displayEnd :: (Input m, Output m) => [Int] -> GameStore -> m ()
displayEnd sequence store = 
  let winner = Map.foldr (\p acc -> if Set.size (ranks p) > Set.size (ranks acc) then p else acc) 
                (players store ! head sequence) (players store) in do
  write $ "\n====================\n🎉🎉 Player " ++ show (pid winner) ++ " won! 🎉🎉\n"
  write "Here are how many ranks everyone collected:"
  write $ getPlayerRanksString store

getPlayerRanksString :: GameStore -> String
getPlayerRanksString store = Map.foldr 
  (\p acc -> let n = Set.size (ranks p) in
    "\nPlayer " ++ show (pid p) ++ " collected " ++ show n ++ " ranks:" 
    ++ (listToString (Set.toList (ranks p)) " :(") ++ "\n" ++ acc)
  "" (players store)

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
               \are either in your hand or laid out).\n\
               \====================\n"

commandsText :: String
commandsText = "Here are commands you can use:\n \
           \help: to view what commands you can use\n \
           \instr: to view Diplomat instructions\n \
           \hand: to view the cards in your hand\n \
           \laidout: to view currently laid-out cards\n \
           \claimed: to view the ranks you have claimed\n \
           \claim: to claim a rank\n \
           \ask: to ask another player a question\n \
           \quit: to quit the game.\n"

introText :: String
introText = "\nWelcome to Diplomat!\n"

promptText :: String
promptText = ">> Please type a command."

summaryText :: String
summaryText = "Summary since your last move:"

claimHelpText :: String
claimHelpText = "Here are commands you can use:\n \
                \help: to view what commands you can use\n \
                \nvm: to stop claiming a rank. \
                \quit: to quit the game.\n"

-- | make moves until someone wins
-- TODO: restrict arguments
play :: (Input m, Output m) => Int -> Int -> m ()
play numPlayers numAI = let initialStore = initialGameStore numPlayers numAI
                            sequence = [0..numPlayers + numAI - 1] in do
  write introText
  write commandsText
  goIntro sequence initialStore

goIntro :: (Input m, Output m) => [Int] -> GameStore -> m ()
goIntro sequence store = 
  let player = players store ! head sequence 
      playerId = pid player in
    do
    if (checkEnd store) then displayEnd sequence store
    else do
      let clearedPrevMoves = clearPrevMoves playerId (prevMoves store)
          store' = store { prevMoves = clearedPrevMoves }
      if (hasPlayerBreak playerId (prevMoves store)) then do
        write $ ">> It's Player " ++ show playerId ++ "'s turn!! Press <enter> to continue."
        _ <- input
        write $ summaryText ++ (listToString (filterMoveBreaks (prevMoves store')) "\nThere is nothing to summarize :o") ++ "\n"
      else write $ "It's still Player " ++ show playerId ++ "'s turn :)\n"
      write promptText
      playerCommand <- input
      case playerCommand of
        "help" -> write ("\n" ++ commandsText ++ "\n") >>
                  goIntro sequence store'
        "instr" -> write ("\n" ++ instructions ++ "\n") >>
                  goIntro sequence store'
        "quit" -> return ()
        "hand" -> write ("\nYour hand:" ++ (listToString (Set.toList (hand player)) "\nYou have no cards in your hand left :o") ++ "\n") >>
                  goIntro sequence store'
        "laidout" -> write ("\nCurrent laid out cards:" ++ (listToString (laidOutCards store) "\nThere are no laid out cards :o") ++ "\n") >>
                    goIntro sequence store'
        "claimed" -> write ("\nYour current claimed ranks:" ++ (listToString (Set.toList (ranks player)) "\nYou haven't claimed any ranks yet :o") ++ "\n") >>
                    goIntro sequence store'
        "claim" -> goClaim sequence store'
        "ask" -> goQuestion sequence store'
        "past" -> write "\nPrev store moves:" >>
                  write (show (prevMoves store) ++ "\n") >>
                  write "\nPrev store' moves [CLEARED]:" >>
                  write (show (prevMoves store') ++ "\n") >>
                  goIntro sequence store'
        "all" -> write "\nEntire game store:" >>
                write (show (players store) ++ "\n") >>
                goIntro sequence store'
        _ -> write "\nNot a valid menu command. Try again!\n" >>
            goIntro sequence store'

goClaim :: (Input m, Output m) => [Int] -> GameStore -> m ()
goClaim sequence store =
  let player = players store ! head sequence in do
    write "\n>> Enter a rank to claim."
    toClaim <- input
    case toClaim of
      "quit" -> return ()
      "help" -> write ("\n" ++ claimHelpText ++ "\n") >>
                goClaim sequence store
      "nvm" -> write "\nNot claiming any ranks." >>
               goIntro sequence store
      _ -> case (readMaybe toClaim :: Maybe Rank) of
              Just r -> case claimRank store player r of
                Just store' -> do
                  write ("\nCongrats, you successfully claimed the rank " ++ show r ++ " :)\n")
                  goIntro sequence store'
                Nothing ->
                  write ("\nYou weren't able to claim the rank " ++ show r ++ " :(\n") >>
                  goIntro sequence store
              _ -> write "\nPlease enter a valid rank:" >>
                  goClaim sequence store

goQuestion :: (Input m, Output m) => [Int] -> GameStore -> m ()
goQuestion sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    let player = players store ! head sequence
        playerId = pid player
    write ("\n>> Player " ++ show playerId ++ ": Enter a player id to choose a player to ask a question to.")
    playerIdToQuestion <- input
    case (readMaybe playerIdToQuestion :: Maybe Int) of
      Just i -> case Map.lookup i (players store) of
        Just playerToQuestion -> do
          write ("\n>> Player " ++ show playerId ++ ": Enter a question.")
          write questionOptionsInitial
          q <- input
          case q of
            "1" -> askSpecificCard player playerToQuestion sequence store
            "2" -> createQuestion player playerToQuestion sequence store
            "quit" -> return () -- quit the game
            "none" -> let store' = store { prevMoves = (prevMoves store) ++ [ (MBreak playerId) ] } in
                      write "\nYou have skipped your turn.\n===========\n" >>
                      goIntro sequence' store' -- skip turn
            _   -> write "\nInvalid question" >> goQuestion sequence store
        Nothing -> write "\nPlease enter a valid player id" >>
                   goQuestion sequence store
      Nothing -> write "\nPlease enter an integer player id" >>
                 goQuestion sequence store

askSpecificCard :: (Input m, Output m) => Player -> Player -> [Int] -> GameStore -> m ()
askSpecificCard askingPlayer askedPlayer sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    write "\n>> Enter a rank."
    rank <- input
    write "\n>> Enter a suit: Diamond, Club, Heart, or Spade."
    suit <- input
    case (readMaybe suit :: Maybe Suit, readMaybe rank :: Maybe Rank) of
      (Just s, Just r) ->
        let c = Card r s
            q = SpecificCard c
            ans =  getAnswer q (hand askedPlayer)
            newPrevMoves = (prevMoves store) ++ [ (MQuestion (pid askingPlayer) q (pid askedPlayer) ans) ]
            in
          write ("\nYou asked Player " ++ show (pid askedPlayer) ++ ": " ++ "\n" ++ (show q)) >>
          if ans 
            then let store' = store { prevMoves = newPrevMoves }
                     store'' = layoutCard store' askedPlayer c in
                 write ("Yes, they had the " ++ show c ++ "\n===========\n") 
                 >> goIntro sequence store''
            else let store' = store { prevMoves = newPrevMoves ++ [ (MBreak (pid askingPlayer)) ] } in
                 write ("No, they didn't have the " ++ show c ++ "\n\n===========\n")
                 >> goIntro sequence' store'
      _ -> write "\nInvalid suit or rank" >> goQuestion sequence store

askComplexQuestion :: (Input m, Output m) => 
  Player -> Player -> [Int] -> GameStore -> m ()
askComplexQuestion askingPlayer askedPlayer sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    let q = currQuestion store
        ans = getAnswer q (hand askedPlayer)
        newPrevMoves = (prevMoves store) ++ [ (MQuestion (pid askingPlayer) q (pid askedPlayer) ans) ]
        store' = if ans then store { prevMoves = newPrevMoves }
                 else store { prevMoves = newPrevMoves ++ [ (MBreak (pid askingPlayer)) ] }
    write ("\nYou asked Player " ++ show (pid askedPlayer) ++ ":")
    write (show (currQuestion store))
    putBool ans
    write "\n===========\n"
    goIntro sequence' store'

putBool :: (Input m, Output m) => Bool -> m ()
putBool = write . showBool

showBool :: Bool -> String
showBool True = "Yes! :)"
showBool False = "No :("

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
        updatedPlayer = P playerId newPlayerHand playerClaimedRanks (ai p)
        newPlayerMap = Map.insert playerId updatedPlayer (players store)
        newPrevMoves = (prevMoves store) ++ [(MClaim playerId r)]
        in
    Just $ G newPlayerMap newLaidOut Blank newPrevMoves
  else Nothing

layoutCard :: GameStore -> Player -> Card -> GameStore
layoutCard store p c = 
  let playerId = pid p
      newPlayerHand = Set.delete c (hand p)
      newLaidOut = c : laidOutCards store
      updatedPlayer = P playerId newPlayerHand (ranks p) (ai p)
      newPlayerMap = Map.insert playerId updatedPlayer (players store)
      newPrevMoves = (prevMoves store) ++ [(MLayout playerId c)]
      in
  G newPlayerMap newLaidOut Blank newPrevMoves

createQuestion :: (Input m, Output m) => Player -> Player -> [Int] -> GameStore -> m ()
createQuestion askingPlayer askedPlayer sequence store = let currQ = currQuestion store in 
  case findBlank currQ of
    1 -> createQuestionMain currQ
    2 -> createQuestionInt currQ
    3 -> createQuestionHand currQ
    _ -> askComplexQuestion askingPlayer askedPlayer sequence store
  where
    createQuestionMain :: (Input m, Output m) => Question -> m ()
    createQuestionMain currQ = do
      write $ "\n>> " ++ (questionOptionsBuilding currQ)
      playerInput <- input
      case readQuestionOptionsBuilding playerInput of
        Nothing -> do
          write "\nInvalid input, try again!"
          createQuestionMain currQ
        Just q -> 
          case buildQuestion currQ q of 
            Nothing -> undefined -- should be unreachable
            Just newQ -> 
              createQuestion askingPlayer askedPlayer sequence (store {currQuestion = newQ})
    createQuestionInt :: (Input m, Output m) => Question -> m ()
    createQuestionInt currQ = do
      write $ "\n>> " ++ (questionIntOptions currQ)
      playerInput <- input
      case readQuestionIntOptions playerInput of
        Nothing -> do 
          write "\nInvalid input, try again!"
          createQuestionInt currQ
        Just (IntVal _) -> do 
          write "\n>> Which integer?"
          i <- input
          case readMaybe i :: Maybe Int of
            Just i' -> let q = IntVal i' in
              case buildQuestionWithQInt currQ q of 
                Nothing -> undefined -- should be unreachable
                Just newQ -> 
                  createQuestion askingPlayer askedPlayer sequence (store {currQuestion = newQ})
            Nothing -> do 
              write "\nNot an integer, try again!"
              createQuestionInt currQ
        Just q -> case buildQuestionWithQInt currQ q of
                Nothing -> undefined -- should be unreachable
                Just newQ -> 
                  createQuestion askingPlayer askedPlayer sequence (store {currQuestion = newQ})
    createQuestionHand :: (Input m, Output m) => Question -> m ()
    createQuestionHand currQ = do 
      write $ "\n>> " ++ (questionHandOptions currQ)
      playerInput <- input
      case readQuestionHandOptions playerInput of
        Nothing -> do 
          write "\nInvalid input, try again!"
          createQuestionHand currQ
        Just (Filter _ qh) -> createQuestionFilter currQ
        Just q -> case buildQuestionWithQHand currQ q of 
          Nothing -> undefined -- should be unreachable
          Just newQ -> createQuestion askingPlayer askedPlayer sequence (store {currQuestion = newQ})
    createQuestionFilter :: (Input m, Output m) => Question -> m ()
    createQuestionFilter currQ = do
      write "\n>> Would you like to filter cards out or filter card in?  \n \
             \1: Filter out \n \
             \2: Filter in"
      playerInput <- input
      case readMaybe playerInput :: Maybe Int of 
        Just 1 -> createQuestionFilterOut currQ Set.empty Set.empty 
        Just 2 -> createQuestionFilterIn currQ Set.empty Set.empty
        _ -> do
          write "\nInvalid input, try again!"
          createQuestionFilter currQ  
    createQuestionFilterOut :: (Input m, Output m) => 
      Question -> Set.Set Suit -> Set.Set Rank -> m ()
    createQuestionFilterOut currQ filteredSuits filteredRanks = do
      write "\nSo far, you've filtered out:"
      write ("Suits: " ++ show filteredSuits)
      write ("Suits: " ++ show filteredRanks)
      write $ "\n>> What would you like to filter out? Enter any rank, suit, or" ++ 
              " 'done' if there's nothing else to filter."
      filterOut <- input
      case (readMaybe filterOut :: Maybe Suit) of
        Just s -> 
          createQuestionFilterOut currQ (Set.insert s filteredSuits) filteredRanks
        Nothing -> case (readMaybe filterOut :: Maybe Rank) of
          Just r -> 
            createQuestionFilterOut currQ filteredSuits 
              (Set.insert r filteredRanks)
          Nothing -> case filterOut of
            "done" -> 
              let filterFunction c = not (suit c `Set.member` filteredSuits) && 
                                     not (rank c `Set.member` filteredRanks)
                  filterQuestion = Filter filterFunction BlankQHand in
              case buildQuestionWithQHand currQ filterQuestion of 
                Nothing -> undefined -- should be unreachable
                Just newQ -> createQuestion askingPlayer askedPlayer sequence 
                             (store {currQuestion = newQ})
            _ -> write "\nInvalid input, try again!" >>
                 createQuestionFilterOut currQ filteredSuits filteredRanks
    createQuestionFilterIn :: (Input m, Output m) => 
      Question -> Set.Set Suit -> Set.Set Rank -> m ()
    createQuestionFilterIn currQ filteredSuits filteredRanks = do
      write "\nSo far, you've filtered in:"
      write (show filteredSuits)
      write (show filteredRanks)
      write $ "\n>> What would you like to filter in? Enter any rank, suit, or" ++ 
              " 'done' if there's nothing else to filter."
      filterOut <- input
      case (readMaybe filterOut :: Maybe Suit) of
        Just s -> 
          createQuestionFilterIn currQ (Set.insert s filteredSuits) filteredRanks
        Nothing -> case (readMaybe filterOut :: Maybe Rank) of
          Just r -> 
            createQuestionFilterIn currQ filteredSuits 
              (Set.insert r filteredRanks)
          Nothing -> case filterOut of
            "done" -> 
              let filterFunction c = (suit c `Set.member` filteredSuits || (Set.null filteredSuits)) && 
                                     (rank c `Set.member` filteredRanks || (Set.null filteredRanks))
                  filterQuestion = Filter filterFunction BlankQHand in
              case buildQuestionWithQHand currQ filterQuestion of 
                Nothing -> undefined -- should be unreachable
                Just newQ -> createQuestion askingPlayer askedPlayer sequence 
                             (store {currQuestion = newQ})
            _ -> write "\nInvalid input, try again!" >>
                 createQuestionFilterIn currQ filteredSuits filteredRanks

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

listToString :: forall a. (Show a) => [a] -> String -> [Char]
listToString [] s = s
listToString l _ = List.foldr (\x acc -> "\n-" ++ (show x) ++ acc) "" l