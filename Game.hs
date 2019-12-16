{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Game where

import qualified State as S (State, runState)
import qualified Data.Maybe (Maybe)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member, null, insert, empty, 
                                  toList)
import qualified Data.Map as Map (lookup)
import Text.Read (readMaybe)
import System.Random (randomIO)

import Question (Question (SpecificCard),
                 QHand (Filter, BlankQHand), 
                 QInt (IntVal), getAnswer, readQuestionOptionsBuilding, 
                 readQuestionHandOptions, readQuestionIntOptions, 
                 questionOptionsBuilding, questionIntOptions, 
                 questionHandOptions, questionOptionsInitial)
import BuildQuestion (buildQuestionWithQHand,
                      buildQuestionWithQInt,
                      buildQuestion,
                      findBlank)
import GamePieces (Suit, Rank, Card (Card), Player, PlayerHand,
                   rank, suit, pid, hand, ranks, ai)
import GameState (Move (MBreak, MQuestion), GameStore,
                  players, laidOutCards, currQuestion, prevMoves,
                  initialGameStore, checkEnd, getPlayerRanksString, getWinner,
                  move, claimRank, layoutCard, hasPlayerBreak, 
                  filterMoveBreaks, clearPrevMoves, prettyPrintList)
import GameTexts
import AI (runAiTurn)

-------------------------------------------------------------------------------
class Monad m => Output m where
  write :: String -> m ()
class Monad m => Input m where
  input :: m String
    
instance Output IO where
  write = putStrLn
instance Input IO where
  input = getLine

-------------------------------------------------------------------------------
-- | main play function, which generates a random seed for shuffling
play :: Int -> Int -> IO ()
play numPlayers numAi = do
  seed <- randomIO :: IO Int
  play' seed numPlayers numAi

-- | play function taking in a seed for random shuffling
play' :: (Input m, Output m) => Int -> Int -> Int -> m ()
play' seed numPlayers numAi = 
  let initialStore = initialGameStore seed numPlayers numAi
      sequence = [0..numPlayers + numAi - 1] in do
  write introText
  write commandsText
  goIntro sequence initialStore

-- | displays game statistics once the game ends
displayEnd :: (Input m, Output m) => [Int] -> GameStore -> m ()
displayEnd sequence store = 
  let winner = getWinner store in do
  write $ endText (pid winner)
  write $ getPlayerRanksString store

-- | start of turn
goIntro :: (Input m, Output m) => [Int] -> GameStore -> m ()
goIntro sequence@(playerId:_) store = if checkEnd store
  then displayEnd sequence store
  else case Map.lookup playerId (players store) of
    Just player -> 
      let clearedPrevMoves = clearPrevMoves playerId (prevMoves store)
          store' = store { prevMoves = clearedPrevMoves } in
      if ai player then goIntroAi sequence store'
      else goIntroPlayer sequence store' 
        (hasPlayerBreak playerId (prevMoves store))
    Nothing -> write errorText
goIntro _ _ = write errorText

-- | start of AI turn
goIntroAi :: (Input m, Output m) => [Int] -> GameStore -> m ()
goIntroAi sequence@(playerId:_) store = 
  let store' = runAiTurn store playerId
      (sequence', _) = S.runState (move sequence) store' in
    goIntro sequence' store'
goIntroAi _ _ = write errorText

-- | start of human player turn
goIntroPlayer :: (Input m, Output m) => [Int] -> GameStore -> Bool -> m ()
goIntroPlayer sequence@(playerId:_) store isNewTurn =
  case Map.lookup playerId (players store) of
    Just player -> do
      if isNewTurn then do
        write $ newTurnText playerId
        _ <- input
        write $ summaryText ++ 
                prettyPrintList (filterMoveBreaks (prevMoves store)) 
                emptySummaryText
      else write $ oldTurnText playerId
      write promptText
      playerCommand <- input
      case playerCommand of
        "help" -> write ("\n" ++ commandsText ++ "\n") >>
                  goIntro sequence store
        "instr" -> write ("\n" ++ instructions ++ "\n") >>
                  goIntro sequence store
        "quit" -> return ()
        "hand" -> write ("\nYour hand:" ++ 
                        prettyPrintList (Set.toList (hand player)) 
                        emptyHandText ++ "\n") >>
                  goIntro sequence store
        "laidout" -> write ("\nCurrent laid-out cards:" ++ 
                            prettyPrintList (laidOutCards store) 
                            emptyLaidOutText ++ "\n") >>
                    goIntro sequence store
        "claimed" -> write ("\nYour current claimed ranks:" ++ 
                            prettyPrintList (Set.toList (ranks player)) 
                            emptyClaimedText ++ "\n") >>
                    goIntro sequence store
        "claim" -> goClaim sequence store
        "ask" -> goQuestion sequence store
        "past" -> write "\nPrev store moves:" >> -- for debugging purposes only
                  write (show (prevMoves store) ++ "\n") >>
                  write "\nPrev store' moves [CLEARED]:" >>
                  write (show (prevMoves store) ++ "\n") >>
                  goIntro sequence store
        "all" -> write "\nEntire game store:" >> -- for debugging purposes only
                write (show (players store) ++ "\n") >>
                goIntro sequence store
        _ -> write "\nNot a valid menu command. Try again!\n" >>
            goIntro sequence store
    Nothing -> write errorText
goIntroPlayer _ _ _ = write errorText

-- | player action to claim rank
goClaim :: (Input m, Output m) => [Int] -> GameStore -> m ()
goClaim sequence@(playerId:_) store =
  case Map.lookup playerId (players store) of
    Just player -> do
      write rankText
      toClaim <- input
      case toClaim of
        "quit" -> write errorText
        "help" -> write ("\n" ++ claimHelpText ++ "\n") >>
                  goClaim sequence store
        "nvm" -> write "\nNot claiming any ranks." >>
                goIntro sequence store
        _ -> case (readMaybe toClaim :: Maybe Rank) of
                Just r -> case claimRank store playerId r of
                  Just store' -> do
                    write $ claimRankSuccessText r
                    goIntro sequence store'
                  Nothing ->
                    write ("\nYou weren't able to claim the rank " ++ show r ++
                          " :(\n") >>
                    goIntro sequence store
                _ -> write "\nPlease enter a valid rank:" >>
                    goClaim sequence store
    Nothing -> write errorText
goClaim _ _ = write errorText

-- | player action to ask another player a question
goQuestion :: (Input m, Output m) => [Int] -> GameStore -> m ()
goQuestion sequence@(playerId:_) store = 
  let (sequence', _) = S.runState (move sequence) store in
    case Map.lookup playerId (players store) of
      Just player -> do
        write $ enterIdText playerId
        playerIdToQuestion <- input
        case (readMaybe playerIdToQuestion :: Maybe Int) of
          Just i -> case Map.lookup i (players store) of
            Just playerToQuestion -> do
              write $ enterQuestionText playerId
              write questionOptionsInitial
              q <- input
              case q of
                "1" -> askSpecificCard player playerToQuestion sequence store
                "2" -> createQuestion player playerToQuestion sequence store
                "quit" -> return () -- quit the game
                "none" -> let store' = store { prevMoves = prevMoves store ++ 
                                              [ MBreak playerId ] } in
                          write ("\nYou have skipped your turn." ++ 
                                 lineBreakText) >>
                          goIntro sequence' store' -- skip turn
                _   -> write "\nInvalid question" >> goQuestion sequence store
            Nothing -> write "\nPlease enter a valid player id" >>
                      goQuestion sequence store
          Nothing -> write "\nPlease enter an integer player id" >>
                    goQuestion sequence store
      Nothing -> write errorText
goQuestion _ _ = write errorText

-- | player action to ask another player if they have a specific card
askSpecificCard :: (Input m, Output m) => 
  Player -> Player -> [Int] -> GameStore -> m ()
askSpecificCard askingPlayer askedPlayer sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    write rankText
    rankIn <- input
    write suitText
    suitIn <- input
    case (readMaybe suitIn :: Maybe Suit, readMaybe rankIn :: Maybe Rank) of
      (Just s, Just r) ->
        let c = Card r s
            q = SpecificCard c
            ans =  getAnswer q (hand askedPlayer)
            newPrevMoves = prevMoves store ++ 
              [ MQuestion (pid askingPlayer) q (pid askedPlayer) ans ]
            in
          write (askText (pid askedPlayer) q) >>
          if ans 
            then let store' = store { prevMoves = newPrevMoves }
                     store'' = layoutCard store' askedPlayer c in
                 write (askSuccessText c) >>
                 goIntro sequence store''
            else let store' = store { prevMoves = newPrevMoves ++ 
                                      [ MBreak (pid askingPlayer) ] } in
                 write (askFailureText c) >>
                 goIntro sequence' store'
      _ -> write "\nInvalid suit or rank" >> goQuestion sequence store

-- | player action to ask another player a complex question
askComplexQuestion :: (Input m, Output m) => 
  Player -> Player -> [Int] -> GameStore -> m ()
askComplexQuestion askingPlayer askedPlayer sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    let q = currQuestion store
        ans = getAnswer q (hand askedPlayer)
        newPrevMoves = prevMoves store ++ 
          [ MQuestion (pid askingPlayer) q (pid askedPlayer) ans,
            MBreak (pid askingPlayer) ]
        store' = store { prevMoves = newPrevMoves }
    write $ askText (pid askedPlayer) q
    write $ showBool ans
    write lineBreakText
    goIntro sequence' store'

-- | Creates a new question by asking the user for input 
-- | until the question is complete.
createQuestion :: (Input m, Output m) => 
  Player -> Player -> [Int] -> GameStore -> m ()
createQuestion askingPlayer askedPlayer sequence store = 
  let currQ = currQuestion store in 
  case findBlank currQ of
    1 -> createQuestionMain currQ
    2 -> createQuestionInt currQ
    3 -> createQuestionHand currQ
    _ -> askComplexQuestion askingPlayer askedPlayer sequence store
  where
    createQuestionMain :: (Input m, Output m) => Question -> m ()
    createQuestionMain currQ = do
      write $ questionOptionsBuilding currQ
      playerInput <- input
      case readQuestionOptionsBuilding playerInput of
        Nothing -> do
          write invalidInputText
          createQuestionMain currQ
        Just q -> 
          case buildQuestion currQ q of 
            Nothing -> write errorText -- should be unreachable
            Just newQ -> 
              createQuestion askingPlayer askedPlayer sequence 
              (store {currQuestion = newQ})
    createQuestionInt :: (Input m, Output m) => Question -> m ()
    createQuestionInt currQ = do
      write $ questionIntOptions currQ
      playerInput <- input
      case readQuestionIntOptions playerInput of
        Nothing -> do 
          write invalidInputText
          createQuestionInt currQ
        Just (IntVal _) -> do 
          write "\n>> Which integer?"
          i <- input
          case readMaybe i :: Maybe Int of
            Just i' -> let q = IntVal i' in
              case buildQuestionWithQInt currQ q of 
                Nothing -> write errorText -- should be unreachable
                Just newQ -> 
                  createQuestion askingPlayer askedPlayer sequence 
                  (store {currQuestion = newQ})
            Nothing -> do 
              write "\nNot an integer, try again!"
              createQuestionInt currQ
        Just q -> case buildQuestionWithQInt currQ q of
                Nothing -> write errorText -- should be unreachable
                Just newQ -> 
                  createQuestion askingPlayer askedPlayer sequence 
                  (store {currQuestion = newQ})
    createQuestionHand :: (Input m, Output m) => Question -> m ()
    createQuestionHand currQ = do 
      write $ questionHandOptions currQ
      playerInput <- input
      case readQuestionHandOptions playerInput of
        Nothing -> do 
          write invalidInputText
          createQuestionHand currQ
        Just (Filter _ qh) -> createQuestionFilter currQ
        Just q -> case buildQuestionWithQHand currQ q of 
          Nothing -> write errorText -- should be unreachable
          Just newQ -> createQuestion askingPlayer askedPlayer sequence 
                       (store {currQuestion = newQ})
    createQuestionFilter :: (Input m, Output m) => Question -> m ()
    createQuestionFilter currQ = do
      write filterOutInText
      playerInput <- input
      case readMaybe playerInput :: Maybe Int of 
        Just 1 -> createQuestionFilterOut currQ Set.empty Set.empty 
        Just 2 -> createQuestionFilterIn currQ Set.empty Set.empty
        _ -> do
          write invalidInputText
          createQuestionFilter currQ  
    createQuestionFilterOut :: (Input m, Output m) => 
      Question -> Set Suit -> Set Rank -> m ()
    createQuestionFilterOut currQ filteredSuits filteredRanks = do
      write $ filteredOutSoFarText filteredSuits filteredRanks
      write filterOutText
      filterOut <- input
      case (readMaybe filterOut :: Maybe Suit) of
        Just s -> 
          createQuestionFilterOut currQ 
            (Set.insert s filteredSuits) filteredRanks
        Nothing -> case (readMaybe filterOut :: Maybe Rank) of
          Just r -> createQuestionFilterOut currQ filteredSuits 
            (Set.insert r filteredRanks)
          Nothing -> case filterOut of
            "done" -> 
              let filterFunction c = not (suit c `Set.member` filteredSuits) &&
                                     not (rank c `Set.member` filteredRanks)
                  filterQuestion = Filter filterFunction BlankQHand in
              case buildQuestionWithQHand currQ filterQuestion of 
                Nothing -> write errorText -- should be unreachable
                Just newQ -> createQuestion askingPlayer askedPlayer sequence 
                             (store {currQuestion = newQ})
            _ -> write invalidInputText >>
                 createQuestionFilterOut currQ filteredSuits filteredRanks
    createQuestionFilterIn :: (Input m, Output m) => 
      Question -> Set Suit -> Set Rank -> m ()
    createQuestionFilterIn currQ filteredSuits filteredRanks = do
      write $ filteredInSoFarText filteredSuits filteredRanks
      write filterInText
      filterOut <- input
      case (readMaybe filterOut :: Maybe Suit) of
        Just s -> createQuestionFilterIn currQ 
          (Set.insert s filteredSuits) filteredRanks
        Nothing -> case (readMaybe filterOut :: Maybe Rank) of
          Just r -> 
            createQuestionFilterIn currQ filteredSuits 
              (Set.insert r filteredRanks)
          Nothing -> case filterOut of
            "done" -> 
              let filterFunction c = 
                    (suit c `Set.member` filteredSuits || 
                      Set.null filteredSuits) &&
                    (rank c `Set.member` filteredRanks || 
                      Set.null filteredRanks)
                  filterQuestion = Filter filterFunction BlankQHand in
              case buildQuestionWithQHand currQ filterQuestion of 
                Nothing -> write errorText -- should be unreachable
                Just newQ -> createQuestion askingPlayer askedPlayer sequence 
                             (store {currQuestion = newQ})
            _ -> write invalidInputText >>
                 createQuestionFilterIn currQ filteredSuits filteredRanks
