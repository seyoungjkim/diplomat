{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Tests where

import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, size, null, toList, member)
import Control.Monad (liftM,liftM2,liftM3)
import Data.Map (Map, (!))
import qualified Data.Map as Map (empty, insert, keys)
import Test.HUnit
import Test.QuickCheck 
import qualified State as S (runState)

import Question
import GamePieces
import GameState
import IOTest (fakeIOTest)

main :: IO ()
main = do
  _ <- runTestTT unitTests
  _ <- runTestTT fakeIOTest
  quickCheck propHandsContainSpecificCard
  quickCheck propNonEmptyHand
  quickCheck propUnionQuestions
  quickCheck propIntersectionQuestions
  quickCheck propNotQuestion
  quickCheck propEqualsQuestion
  quickCheck propEqualsQuestionSame
  quickCheck propGeQuestionSame
  quickCheck propGtQuestionSame
  quickCheck propLtQuestionSame
  quickCheck propLeQuestionSame
  quickCheck propDeMorgansLaw1
  quickCheck propDeMorgansLaw2
  quickCheck propIntVal
  quickCheck propCardinalitySimple
  quickCheck propAllCardsDistributed
  quickCheck propCardsEvenDistributed
  quickCheck propNoFaceUpOnStart
  return ()

-------------------- Question Tests --------------------

-- for all cards c in a hand h, getAnswer (SpecificCard c) h is true iff
-- the hand contains c
propHandsContainSpecificCard :: PlayerHand -> Bool
propHandsContainSpecificCard h =
  all (\c -> Set.member c h == getAnswer (SpecificCard c) h) deck 

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

-- the negation of a question always returns the opposite result, for all hands
propNotQuestion :: Question -> PlayerHand -> Bool
propNotQuestion q h = getAnswer q h /= getAnswer (Not q) h

-- the equals method works as expected
propEqualsQuestion :: QInt -> QInt -> PlayerHand -> Bool
propEqualsQuestion qi1 qi2 h = 
  (getAnswerInt qi1 h == getAnswerInt qi2 h) == getAnswer (Equals qi1 qi2) h

-- equals always returns true if given two identical questions
propEqualsQuestionSame :: QInt -> PlayerHand -> Bool
propEqualsQuestionSame qi = getAnswer (Equals qi qi)

-- gt always returns false if given two identical questions
propGtQuestionSame :: QInt -> PlayerHand -> Bool
propGtQuestionSame qi h = not $ getAnswer (Gt qi qi) h

-- ge always returns true if given two identical questions
propGeQuestionSame :: QInt -> PlayerHand -> Bool
propGeQuestionSame qi = getAnswer (Ge qi qi)

-- lt always returns false if given two identical questions
propLtQuestionSame :: QInt -> PlayerHand -> Bool
propLtQuestionSame qi h = not $ getAnswer (Lt qi qi) h

-- le always returns true if given two identical questions
propLeQuestionSame :: QInt -> PlayerHand -> Bool
propLeQuestionSame qi = getAnswer (Le qi qi)

propDeMorgansLaw1 :: Question -> Question -> PlayerHand -> Bool
propDeMorgansLaw1 q1 q2 h = getAnswer (Not (Union q1 q2)) h == 
  getAnswer (Intersection (Not q1) (Not q2)) h

propDeMorgansLaw2 :: Question -> Question -> PlayerHand -> Bool
propDeMorgansLaw2 q1 q2 h = getAnswer (Not (Intersection q1 q2)) h == 
  getAnswer (Union (Not q1) (Not q2)) h

propIntVal :: Int -> PlayerHand -> Bool
propIntVal i h = i == getAnswerInt (IntVal i) h

propCardinalitySimple :: PlayerHand -> Bool
propCardinalitySimple h = getAnswerInt (Cardinality Hand) h == length h

instance Arbitrary Card where
  arbitrary = elements deck
  shrink c = []

instance Arbitrary Question where
  arbitrary = frequency [ (1, fmap SpecificCard arbitrary), 
                          (4, fmap Question.NonEmpty arbitrary),
                          (2, liftM2 Union arbitrary arbitrary),
                          (2, liftM2 Intersection arbitrary arbitrary),
                          (2, fmap Not arbitrary),
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
  arbitrary = frequency [ (2, fmap IntVal arbitrary),
                          (5, fmap Cardinality arbitrary),
                          (1, fmap SumHand arbitrary),
                          (1, fmap ProductHand arbitrary),
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
                          (1, liftM2 IntersectionHand arbitrary arbitrary),
                          (5, liftM2 Filter arbitrary arbitrary) ]
  shrink (UnionHand qh1 qh2) = [qh1, qh2]
  shrink (IntersectionHand qh1 qh2) = [qh1, qh2]
  shrink (Filter _ qh) = [qh]
  shrink _ = []
  
instance CoArbitrary Card where
  coarbitrary c = variant $ fromEnum (suit c) * 13 + fromEnum (rank c)

-------------------- GameState Tests --------------------
-- all cards distributed when game is initialized
propAllCardsDistributed :: Int -> Int -> Int -> Property
propAllCardsDistributed s n a = n >= 0 && a >= 0 && (n + a > 0) ==> 
  let gs = initialGameStore s n a
      allCards :: [Card]
      allCards = 
        Prelude.foldr (\p acc -> Set.toList (hand p) ++ acc) [] (players gs) in
  length allCards == 52 && Set.size (Set.fromList allCards) == 52

-- cards evenly distributed when game is initialized
propCardsEvenDistributed :: Int -> Int -> Int -> Bool
propCardsEvenDistributed s n a = 
  let n' = n `mod` 46 + 1
      a' = a `mod` 46 + 1
      gs = initialGameStore s n' a'
      maxSize = 
        Prelude.foldr (\p acc -> max (Set.size (hand p)) acc) 0 (players gs)
      minSize = 
        Prelude.foldr 
        (\p acc -> min (Set.size (hand p)) acc) 52 (players gs) in
  (maxSize == minSize || maxSize - 1 == minSize) && 
  Prelude.foldr 
    (\p acc -> Set.size (hand p) > 0 && Set.size (hand p) <= 52 && acc) 
    True (players gs)

-- no face up cards when game initialized
propNoFaceUpOnStart :: Int -> Int -> Int -> Property
propNoFaceUpOnStart s n a = n >= 0 && a >= 0 ==> 
  let gs = initialGameStore s n a in
  Prelude.null (laidOutCards gs)

-- all unit tests
unitTests :: Test
unitTests = TestList [
  testPlayerTurn,
  testPlayerTurnAllGo,
  testCheckWin,
  testCheckTie,
  testWinPrint,
  testCorrectPlayerWin,
  testClaimRankLayOut,
  testClaimRank,
  testLayOut ]

-- unit test for the right person's turn
testPlayerTurn :: Test
testPlayerTurn = 
  let gs = initialGameStore 0 4 0
      firstPlayerId = pid (players gs ! 0)
      (ps', gs') = S.runState (move (Map.keys (players gs))) gs
      secondPlayerId = head ps' in
  TestList [firstPlayerId ~?= 0, secondPlayerId ~?= 1]

-- unit test for the right person's turn after going around all players
testPlayerTurnAllGo :: Test
testPlayerTurnAllGo = 
  let gs = initialGameStore 0 4 0
      firstPlayerId = pid (players gs ! 0)
      (ps2, gs2) = S.runState (move (Map.keys (players gs))) gs
      (ps3, gs3) = S.runState (move ps2) gs2
      (ps4, gs4) = S.runState (move ps3) gs3
      (ps5, gs5) = S.runState (move ps4) gs4
      lastPlayerId = head ps5 in
  lastPlayerId ~?= firstPlayerId

-- unit test for the end game where a player wins
testCheckWin :: Test
testCheckWin = checkEnd winState ~?= True

-- unit test for the end game where there is a tie
testCheckTie :: Test
testCheckTie = checkEnd tieState ~?= True

-- unit test for what gets printed at the end
testWinPrint :: Test
testWinPrint = getPlayerRanksString winState ~?= 
  "\nPlayer 0 collected 6 ranks:\n\
  \-Ace\n-Two\n-Three\n-Four\n-Five\n-Six\n\n\
  \Player 1 collected 6 ranks:\n\
  \-Seven\n-Eight\n-Nine\n-Ten\n-Jack\n-Queen\n\n\
  \Player 2 collected 1 ranks:\n\
  \-King\n\n\
  \Player 3 collected 0 ranks: :(\n"

-- unit test that player 0 wins the winState
testCorrectPlayerWin :: Test
testCorrectPlayerWin = pid (getWinner winState) ~?= 0

-- unit test to check that ranks are claimed correctly
testClaimRank :: Test
testClaimRank =
  let store = fakeGameAllCards in
  case claimRank store (pid (players store ! 0)) Ace of
    Just store' -> TestList [ ranks (players store ! 0) ~?= Set.empty,
                Set.size (hand (players store ! 0)) ~?= 52,
                ranks (players store' ! 0) ~?= Set.fromList [Ace],
                Set.size (hand (players store' ! 0)) ~?= 48 ]
    Nothing -> True ~?= False --failure case

-- write test for ranks being claimed with laid out cards
testClaimRankLayOut :: Test
testClaimRankLayOut = 
  let store  = unshuffledGame
      card = Card Ace Heart
      store2 = layoutCard store (players store ! 0) card in
  case claimRank store2 (pid (players store2 ! 0)) Ace of
    Just store3 -> 
      TestList [
      Prelude.null (laidOutCards store) ~?= True, 
      laidOutCards store2 ~?= [card], 
      Prelude.null (laidOutCards store3) ~?= True, 
      Set.size (hand (players store ! 0)) - 4 ~?= 
        Set.size (hand (players store3 ! 0)), 
      ranks (players store3 ! 0) ~?= Set.fromList [Ace] ]
    Nothing -> True ~?= False --error case
  
-- unit test to check that cards are laid out correctly
testLayOut :: Test
testLayOut = 
  let gs  = unshuffledGame
      card = Card Ace Heart
      gs2 = layoutCard gs (players gs ! 0) card in
  TestList [ 
  Prelude.null (laidOutCards gs) ~?= True,
  laidOutCards gs2 ~?= [card],
  Set.size (hand (players gs ! 0)) - 1 ~?= Set.size (hand (players gs2 ! 0))]


-------------- Helper functions to make writing test cases easier -------------
winState :: GameStore
winState = 
  let n' = 4
      pids = [0..3]
      ranks = [Set.fromList [Ace ..Six], Set.fromList [Seven ..Queen], 
               Set.fromList [King], Set.empty]
      players = createPlayersWin pids ranks in
  G players [] Blank []
  where
    createPlayersWin :: [Int] -> [Set Rank] -> Map Int Player
    createPlayersWin (id : ids) (r : ranks) = 
      Map.insert id (P id Set.empty r False Map.empty) 
      (createPlayersWin ids ranks)
    createPlayersWin _ _ = Map.empty

tieState :: GameStore
tieState =
  let n' = 4
      pids = [0..3]
      ranks = [Set.fromList [Ace ..], Set.empty, Set.empty, Set.empty]
      players = createPlayersWin pids ranks in
  G players [] Blank []
  where
    createPlayersWin :: [Int] -> [Set Rank] -> Map Int Player
    createPlayersWin (id : ids) (r : ranks) = 
      Map.insert id (P id Set.empty r False Map.empty) (
        createPlayersWin ids ranks)
    createPlayersWin _ _ = Map.empty

-- p1 has all diamonds, p2 has all clubs, p3 has all hearts, p4 has all spades
unshuffledGame :: GameStore
unshuffledGame =
  let n' = 4
      pids = [0..3]
      hands = dealDeckUnshuffled n' deck
      players = createPlayersUnshuffled pids (Prelude.take n' hands) in
  G players [] Blank []
  where
    createPlayersUnshuffled :: [Int] -> [PlayerHand] -> Map Int Player
    createPlayersUnshuffled (id : ids) (h : hands) = 
      Map.insert id (P id h Set.empty False Map.empty) 
      (createPlayersUnshuffled ids hands)
    createPlayersUnshuffled _ _ = Map.empty


-- | shuffle and deal deck to given number of players
dealDeckUnshuffled :: Int -> [Card] -> [PlayerHand]
dealDeckUnshuffled n cs = 
                let d = 52 `div` n
                    m = 52 `mod` n 
                    r = m * (d + 1) in
  if m > 0 
    then deal (d + 1) (Prelude.take r cs) ++ deal d (Prelude.drop r cs)
  else deal d cs
  where
    deal _ [] = []
    deal n l = let (hd, tl) = Prelude.splitAt n l in
      Set.fromList hd : deal n tl


-- p1 has all cards
fakeGameAllCards :: GameStore
fakeGameAllCards =
  let n' = 4
      pids = [0..3]
      hands = [Set.fromList deck, Set.empty, Set.empty, Set.empty]
      players = createPlayersUnshuffled pids (Prelude.take n' hands) in
  G players [] Blank []
  where
    createPlayersUnshuffled :: [Int] -> [PlayerHand] -> Map Int Player
    createPlayersUnshuffled (id : ids) (h : hands) = 
      Map.insert id (P id h Set.empty False Map.empty) 
      (createPlayersUnshuffled ids hands)
    createPlayersUnshuffled _ _ = Map.empty

