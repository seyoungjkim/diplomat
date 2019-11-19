{-# OPTIONS -Wincomplete-patterns #-}
module Tests where
import GameState
import Question
import Game
import Data.Set as Set

import Test.HUnit
import Test.QuickCheck

-------------------- Question Tests --------------------
-- union of questions contains all valid cards in both questions
propUnionQuestions :: Question -> Question -> Set Card -> Property
propUnionQuestions q1 q2 hand = undefined

-- intersection of questions contains only valid cards in both questions
propIntersectionQuestions :: Question -> Question -> Set Card -> Property
propIntersectionQuestions = undefined

-- exactly one card fits
propSpecificCard :: Card -> Bool
propSpecificCard c = all (\d -> (d == c) == getAnswer q (Set.singleton d)) deck where 
    q = (specificQuestion c)

instance Arbitrary Question where
  arbitrary = undefined
  shrink = undefined

-------------------- GameState Tests --------------------
-- all cards distributed when game is initialized
propAllCardsDistributed :: Game -> Bool
propAllCardsDistributed = undefined

-- cards evenly distributed when game is initialized
propCardsEvenDistributed :: Game -> Bool
propAllCardsEvenDistributed = undefined

-- unit test for the end game
testCheckEnd :: Test
testCheckEnd = TestList []

-- unit test to check that ranks are claimed correctly
testClaimRank :: Test
testClaimRank = TestList []

-- unit test that cards are laid out correctly // prop?
testLayOut :: Test
testLayOut = TestList []

instance Arbitrary Player where
  arbitrary = undefined

-- Helper functions to make writing test cases easier ---
makeGame :: [Player] -> Game
makeGame = undefined

winState :: Game
winState = undefined

tieState :: Game
tieState = undefined
