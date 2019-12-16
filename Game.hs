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
import GameState
import AI

class Monad m => Output m where
  write :: String -> m ()
class Monad m => Input m where
  input :: m String    -- only return input if it is ready
    
instance Output IO where
  write = putStrLn
instance Input IO where
  input = getLine

-------------------------------------------------------------------------------

-- | make moves until someone wins
play :: (Input m, Output m) => Int -> Int -> m ()
play numPlayers numAI = let initialStore = initialGameStore numPlayers numAI
                            sequence = [0..numPlayers + numAI - 1] in do
  write introText
  write commandsText
  goIntro sequence initialStore

-- | displays game statistics once the game ends
displayEnd :: (Input m, Output m) => [Int] -> GameStore -> m ()
displayEnd sequence store = 
  let winner = getWinner store in do
  write "\n===================="
  write $ "🎉🎉 Player " ++ show (pid winner) ++ " won! 🎉🎉\n"
  write "Here are how many ranks everyone collected:"
  write $ getPlayerRanksString store

goIntro :: (Input m, Output m) => [Int] -> GameStore -> m ()
goIntro sequence store = if (checkEnd store) then displayEnd sequence store
  else let player = players store ! head sequence 
           playerId = pid player
           clearedPrevMoves = clearPrevMoves playerId (prevMoves store)
           store' = store { prevMoves = clearedPrevMoves } in
       if ai (players store ! head sequence) then goIntroAI sequence store'
       else goIntroPlayer sequence store' 
         (hasPlayerBreak playerId (prevMoves store))

goIntroAI :: (Input m, Output m) => [Int] -> GameStore -> m ()
goIntroAI sequence store = 
  let player = players store ! head sequence 
      store' = runAiTurn store player
      (sequence', _) = S.runState (move sequence) store' in
    goIntro sequence' store'

goIntroPlayer :: (Input m, Output m) => [Int] -> GameStore -> Bool -> m ()
goIntroPlayer sequence store isNewTurn =
  let player = players store ! head sequence 
      playerId = pid player in do
    if isNewTurn then do
      write $ ">> It's Player " ++ show playerId ++ "'s turn!! \ 
              \Press <enter> to continue."
      _ <- input
      write $ summaryText ++ 
              (prettyPrintList (filterMoveBreaks (prevMoves store)) 
              "\nThere is nothing to summarize :o") ++ "\n"
    else write $ "It's still Player " ++ show playerId ++ "'s turn :)\n"
    write promptText
    playerCommand <- input
    case playerCommand of
      "help" -> write ("\n" ++ commandsText ++ "\n") >>
                goIntro sequence store
      "instr" -> write ("\n" ++ instructions ++ "\n") >>
                goIntro sequence store
      "quit" -> return ()
      "hand" -> write ("\nYour hand:" ++ 
                      (prettyPrintList (Set.toList (hand player)) 
                      "\nYou have no cards in your hand left :o") ++ "\n") >>
                goIntro sequence store
      "laidout" -> write ("\nCurrent laid out cards:" ++ 
                          (prettyPrintList (laidOutCards store) 
                          "\nThere are no laid out cards :o") ++ "\n") >>
                  goIntro sequence store
      "claimed" -> write ("\nYour current claimed ranks:" ++ 
                          (prettyPrintList (Set.toList (ranks player)) 
                          "\nYou haven't claimed any ranks yet :o") ++ 
                          "\n") >>
                  goIntro sequence store
      "claim" -> goClaim sequence store
      "ask" -> goQuestion sequence store
      "past" -> write "\nPrev store moves:" >> -- for debugging purposes only
                write (show (prevMoves store) ++ "\n") >>
                write "\nPrev store' moves [CLEARED]:" >>
                write (show (prevMoves store) ++ "\n") >>
                goIntro sequence store
      "all" -> write "\nEntire game store:" >>  -- for debugging purposes only
              write (show (players store) ++ "\n") >>
              goIntro sequence store
      _ -> write "\nNot a valid menu command. Try again!\n" >>
          goIntro sequence store

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
                  write ("\nCongrats, you successfully claimed the rank " ++ 
                        show r ++ " :)\n")
                  goIntro sequence store'
                Nothing ->
                  write ("\nYou weren't able to claim the rank " ++ show r ++
                        " :(\n") >>
                  goIntro sequence store
              _ -> write "\nPlease enter a valid rank:" >>
                  goClaim sequence store

goQuestion :: (Input m, Output m) => [Int] -> GameStore -> m ()
goQuestion sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    let player = players store ! head sequence
        playerId = pid player
    write ("\n>> Player " ++ show playerId ++ 
          ": Enter a player id to choose a player to ask a question to.")
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
            "none" -> let store' = store { prevMoves = (prevMoves store) ++ 
                                           [ (MBreak playerId) ] } in
                      write "\nYou have skipped your turn.\n===========\n" >>
                      goIntro sequence' store' -- skip turn
            _   -> write "\nInvalid question" >> goQuestion sequence store
        Nothing -> write "\nPlease enter a valid player id" >>
                   goQuestion sequence store
      Nothing -> write "\nPlease enter an integer player id" >>
                 goQuestion sequence store

askSpecificCard :: (Input m, Output m) => 
  Player -> Player -> [Int] -> GameStore -> m ()
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
            newPrevMoves = (prevMoves store) ++ 
              [ (MQuestion (pid askingPlayer) q (pid askedPlayer) ans) ]
            in
          write ("\nYou asked Player " ++ show (pid askedPlayer) ++ 
                ": " ++ "\n" ++ (show q)) >>
          if ans 
            then let store' = store { prevMoves = newPrevMoves }
                     store'' = layoutCard store' askedPlayer c in
                 write ("Yes, they had the " ++ show c ++ "\n===========\n") 
                 >> goIntro sequence store''
            else let store' = store { prevMoves = newPrevMoves ++ 
                                      [ (MBreak (pid askingPlayer)) ] } in
                 write ("No, they didn't have the " ++ 
                       show c ++ "\n\n===========\n")
                 >> goIntro sequence' store'
      _ -> write "\nInvalid suit or rank" >> goQuestion sequence store

askComplexQuestion :: (Input m, Output m) => 
  Player -> Player -> [Int] -> GameStore -> m ()
askComplexQuestion askingPlayer askedPlayer sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    let q = currQuestion store
        ans = getAnswer q (hand askedPlayer)
        newPrevMoves = (prevMoves store) ++ 
          [ (MQuestion (pid askingPlayer) q (pid askedPlayer) ans) ]
        store' = if ans then store { prevMoves = newPrevMoves }
                 else store { prevMoves = newPrevMoves ++ 
                              [ (MBreak (pid askingPlayer)) ] }
    write ("\nYou asked Player " ++ show (pid askedPlayer) ++ ":")
    write (show (currQuestion store))
    write $ showBool ans
    write "\n===========\n"
    goIntro sequence' store'

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
