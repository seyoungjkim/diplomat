{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module IOTest where

import GameTexts
import Game (Input, Output, input, write, play')
import GamePieces (Card (Card), Suit (Spade, Club), Rank (Queen, King), suit)
import Question (Question (Blank, NonEmpty, SpecificCard),
                 QHand (BlankQHand, Hand, Filter),
                 questionOptionsInitial, questionHandOptions, 
                 questionOptionsBuilding)

import Test.HUnit
import qualified State as S (State, execState, put, get)
import qualified Data.DList as DL (DList, empty, singleton, append, toList)
import Data.Set as Set (empty, singleton)

-------------------- Fake IO --------------------

type FakeIO = S.State FakeState

data FakeState = FS
  { fsWrite :: DL.DList String    -- what has been written
  , fsInput :: [String]     -- what to read from
  }

instance Output FakeIO where
  write s = do
    st <- S.get
    let oldLog = fsWrite st
    let newLog = DL.append oldLog (DL.singleton s)
    S.put $ st { fsWrite = newLog }

instance Input FakeIO where
  input = do
    st <- S.get
    let (v,rest) = case fsInput st of
                     []     -> ("",[])
                     (x:xs) -> (x,xs)
    S.put $ st { fsInput = rest }
    return v

runFakeIO :: FakeIO () -> [String] -> [String]
runFakeIO comp inputs =
    DL.toList (fsWrite (S.execState comp initState))
  where
    initState = FS { fsWrite = DL.empty, fsInput = inputs }

fakeIOTest :: Test
fakeIOTest =
  runFakeIO (play' 0 1 1)
    [ -- Player 0 asks 2 for Queen of Spades, answer is yes
      "","ask","1","1","Queen","Spade",
      -- Player 0 claims Queen
      "claim","Queen",
      -- Player 0 asks 1 if they have any clubs, answer is yes
      "ask","1","2","1","2","2","Club","done","1",
      -- AI Player 1 runs, Player 0 asks 1 if they have clubs, answer is yes
      "","ask","1","1","King", "Spade",
      -- Player 0 claims King, game ends
      "claim","King"
    ] ~?= 
    [ -- Player 0 starts
      introText, commandsText, newTurnText 0, 
      -- "" (Player 0)
      summaryText ++ emptySummaryText, promptText, 
      -- "ask"
      enterIdText 0, 
      -- "1" (asking Player 1)
      enterQuestionText 0, questionOptionsInitial, 
      -- "1" (asking question type 1, specific card)
      rankText, 
      -- "Queen"
      suitText, 
      -- "Spade"
      askText 1 (SpecificCard (Card Queen Spade)), 
      askSuccessText (Card Queen Spade),
      oldTurnText 0, promptText, 
      -- "claim"
      rankText, 
      -- "Queen"
      claimRankSuccessText Queen, oldTurnText 0, promptText, 
      -- "ask"
      enterIdText 0,  
      -- "1" (asking Player 1)
      enterQuestionText 0, questionOptionsInitial, 
      -- "2" (asking question type, complex card)
      questionOptionsBuilding Blank, 
      -- "1" (non-empty)
      questionHandOptions $ Question.NonEmpty Question.BlankQHand, 
      -- "2" (filter)
      filterOutInText, 
      -- "2" (filter in)
      filteredInSoFarText Set.empty Set.empty, filterInText, 
      -- "Club"
      filteredInSoFarText (Set.singleton Club) Set.empty, filterInText, 
      -- "done"
      questionHandOptions (Question.NonEmpty 
        (Filter (\c -> suit c == Club) Question.BlankQHand)), 
      -- "1" (Hand)
      askText 1 (Question.NonEmpty (Filter (\c -> suit c == Club) Hand)), 
      showBool True,
      lineBreakText, newTurnText 0, 
      -- "" (Player 0)
      summaryText ++ summary, promptText, 
      -- "ask"
      enterIdText 0, 
      -- "1" (asking Player 1)
      enterQuestionText 0, questionOptionsInitial, 
      -- "1" (asking question type 1, specific card)
      rankText, 
      -- "King"
      suitText, 
      -- "Spade"
      askText 1 (SpecificCard (Card King Spade)), 
      askSuccessText (Card King Spade),
      oldTurnText 0, promptText, 
      -- "claim"
      rankText, -- "King"
      claimRankSuccessText King, endText 1, stats
    ]

summary :: String
summary = "\n-Player 1 claimed the Aces!\n\
  \-Player 1 asked Player 0 <Do you have the Two of Diamonds?> and they answered Yes! :)\n\
  \-Player 0 laid out the Two of Diamonds.\n\
  \-Player 1 asked Player 0 <Do you have the Two of Hearts?> and they answered Yes! :)\n\
  \-Player 0 laid out the Two of Hearts.\n\
  \-Player 1 claimed the Twos!\n\
  \-Player 1 asked Player 0 <Do you have the Three of Diamonds?> and they answered Yes! :)\n\
  \-Player 0 laid out the Three of Diamonds.\n\
  \-Player 1 asked Player 0 <Do you have the Three of Clubs?> and they answered Yes! :)\n\
  \-Player 0 laid out the Three of Clubs.\n\
  \-Player 1 asked Player 0 <Do you have the Three of Hearts?> and they answered Yes! :)\n\
  \-Player 0 laid out the Three of Hearts.\n\
  \-Player 1 claimed the Threes!\n\
  \-Player 1 asked Player 0 <Do you have the Four of Spades?> and they answered Yes! :)\n\
  \-Player 0 laid out the Four of Spades.\n\
  \-Player 1 claimed the Fours!\n\
  \-Player 1 asked Player 0 <Do you have the Five of Diamonds?> and they answered Yes! :)\n\
  \-Player 0 laid out the Five of Diamonds.\n\
  \-Player 1 asked Player 0 <Do you have the Five of Hearts?> and they answered Yes! :)\n\
  \-Player 0 laid out the Five of Hearts.\n\
  \-Player 1 asked Player 0 <Do you have the Five of Spades?> and they answered Yes! :)\n\
  \-Player 0 laid out the Five of Spades.\n\
  \-Player 1 claimed the Fives!\n\
  \-Player 1 asked Player 0 <Do you have the Six of Diamonds?> and they answered Yes! :)\n\
  \-Player 0 laid out the Six of Diamonds.\n\
  \-Player 1 asked Player 0 <Do you have the Six of Clubs?> and they answered Yes! :)\n\
  \-Player 0 laid out the Six of Clubs.\n\
  \-Player 1 claimed the Sixs!\n\
  \-Player 1 asked Player 0 <Do you have the Seven of Diamonds?> and they answered Yes! :)\n\
  \-Player 0 laid out the Seven of Diamonds.\n\
  \-Player 1 asked Player 0 <Do you have the Seven of Clubs?> and they answered Yes! :)\n\
  \-Player 0 laid out the Seven of Clubs.\n\
  \-Player 1 claimed the Sevens!\n\
  \-Player 1 asked Player 0 <Do you have the Eight of Diamonds?> and they answered Yes! :)\n\
  \-Player 0 laid out the Eight of Diamonds.\n\
  \-Player 1 asked Player 0 <Do you have the Eight of Clubs?> and they answered Yes! :)\n\
  \-Player 0 laid out the Eight of Clubs.\n\
  \-Player 1 claimed the Eights!\n\
  \-Player 1 asked Player 0 <Do you have the Nine of Clubs?> and they answered Yes! :)\n\
  \-Player 0 laid out the Nine of Clubs.\n\
  \-Player 1 claimed the Nines!\n\
  \-Player 1 asked Player 0 <Do you have the Ten of Diamonds?> and they answered Yes! :)\n\
  \-Player 0 laid out the Ten of Diamonds.\n\
  \-Player 1 asked Player 0 <Do you have the Ten of Clubs?> and they answered Yes! :)\n\
  \-Player 0 laid out the Ten of Clubs.\n\
  \-Player 1 claimed the Tens!\n\
  \-Player 1 asked Player 0 <Do you have the Jack of Clubs?> and they answered Yes! :)\n\
  \-Player 0 laid out the Jack of Clubs.\n\
  \-Player 1 asked Player 0 <Do you have the Jack of Hearts?> and they answered Yes! :)\n\
  \-Player 0 laid out the Jack of Hearts.\n\
  \-Player 1 claimed the Jacks!\n\
  \-Player 1 asked Player 0 <Do you have the Queen of Diamonds?> and they answered No :("

stats :: String
stats = "\nPlayer 0 collected 2 ranks:\n\
  \-Queen\n\
  \-King\n\n\
  \Player 1 collected 11 ranks:\n\
  \-Ace\n\
  \-Two\n\
  \-Three\n\
  \-Four\n\
  \-Five\n\
  \-Six\n\
  \-Seven\n\
  \-Eight\n\
  \-Nine\n\
  \-Ten\n\
  \-Jack\n"