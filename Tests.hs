{-# OPTIONS -Wincomplete-patterns #-}
module Tests where
import GameState
import Question
import Game
import Data.Set as Set
import Control.Monad (liftM,liftM2,liftM3)

import Test.HUnit
import Test.QuickCheck

-------------------- Question Tests --------------------

-- for all cards c in a hand h, getAnswer (SpecificCard c) h is true iff
-- the hand contains c
propHandsContainSpecificCard :: PlayerHand -> Bool
propHandsContainSpecificCard h =
  all (\c -> (Set.member c h) == getAnswer (SpecificCard c) h) deck 

-- non-empty hand returns true, else false
propNonEmptyHand :: PlayerHand -> Bool
propNonEmptyHand h = not (Set.null h) == getAnswer (Question.NonEmpty Hand) h

-- union of questions contains all valid cards in both questions
propUnionQuestions :: Question -> Question -> PlayerHand -> Bool
propUnionQuestions q1 q2 h = 
  (getAnswer q1 h || getAnswer q2 h) == getAnswer (Union q1 q2) h

-- intersection of questions contains only valid cards in both questions
propIntersectionQuestions :: Question -> Question -> PlayerHand -> Bool
propIntersectionQuestions q1 q2 h =
  (getAnswer q1 h && getAnswer q2 h) == getAnswer (Intersection q1 q2) h

instance Arbitrary Card where
  arbitrary = elements deck
  shrink c = []

instance Arbitrary Question where
  arbitrary = frequency [ (1, liftM SpecificCard arbitrary), 
                          (4, liftM Question.NonEmpty arbitrary),
                          (2, liftM2 Union arbitrary arbitrary),
                          (2, liftM2 Intersection arbitrary arbitrary),
                          (2, liftM Not arbitrary),
                          (2, liftM2 Equals arbitrary arbitrary),
                          (2, liftM2 Gt arbitrary arbitrary),
                          (2, liftM2 Lt arbitrary arbitrary),
                          (2, liftM2 Ge arbitrary arbitrary),
                          (2, liftM2 Le arbitrary arbitrary) ]                         
   
  shrink (Union q1 q2) = [q1, q2]
  shrink (Intersection q1 q2) = [q1, q2]
  shrink (Not q) = [q]
  shrink _ = []

instance Arbitrary QInt where
  arbitrary = frequency [ (2, liftM IntVal arbitrary),
                          (5, liftM Cardinality arbitrary),
                          (1, liftM SumHand arbitrary),
                          (1, liftM ProductHand arbitrary),
                          (1, liftM2 Sum arbitrary arbitrary),
                          (1, liftM2 Diff arbitrary arbitrary),
                          (3, liftM2 Mod arbitrary arbitrary),
                          (1, liftM2 Product arbitrary arbitrary),
                          (1, liftM2 Quotient arbitrary arbitrary) ]
  shrink (Sum q1 q2) = [q1, q2]
  shrink (Diff q1 q2) = [q1, q2]
  shrink (Mod q1 q2) = [q1, q2]
  shrink (Product q1 q2) = [q1, q2]
  shrink (Quotient q1 q2) = [q1, q2]
  shrink _ = []

instance Arbitrary QHand where
  arbitrary = frequency [ (8, return Hand),
                          (1, liftM2 UnionHand arbitrary arbitrary),
                          (1, liftM2 IntersectionHand arbitrary arbitrary) ]
  shrink (UnionHand qh1 qh2) = [qh1, qh2]
  shrink (IntersectionHand qh1 qh2) = [qh1, qh2]
  shrink _ = []

-------------------- GameState Tests --------------------
-- all cards distributed when game is initialized
propAllCardsDistributed :: Game -> Bool
propAllCardsDistributed = undefined

-- cards evenly distributed when game is initialized
propCardsEvenDistributed :: Game -> Bool
propCardsEvenDistributed = undefined

-- unit test for the right person's turn
testPlayerTurn :: Test
testPlayerTurn = TestList []

-- unit test for correct answer when player asks question
testCorrectAnswer :: Test
testCorrectAnswer = TestList []

-- unit test for the end game where a player wins
testCheckWin :: Test
testCheckWin = TestList []

-- unit test for the end game where there is a tie
testCheckTie :: Test
testCheckTie = TestList []

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
