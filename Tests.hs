{-# OPTIONS -Wincomplete-patterns #-}
module Tests where
import GameState
import Question
import Game
import Data.Set as Set
import Control.Monad (liftM,liftM2,liftM3)

import Test.HUnit
import Test.QuickCheck
import qualified State as S

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

-- the negation of a question always returns the opposite result, for all hands
propNotQuestion :: Question -> PlayerHand -> Bool
propNotQuestion q h = getAnswer q h /= getAnswer (Not q) h

-- the equals method works as expected
propEqualsQuestion :: QInt -> QInt -> PlayerHand -> Bool
propEqualsQuestion qi1 qi2 h = 
  (getAnswerInt qi1 h == getAnswerInt qi2 h) == getAnswer (Equals qi1 qi2) h

-- equals always returns true if given two identical questions
propEqualsQuestionSame :: QInt -> PlayerHand -> Bool
propEqualsQuestionSame qi h = getAnswer (Equals qi qi) h

-- gt always returns false if given two identical questions
propGtQuestionSame :: QInt -> PlayerHand -> Bool
propGtQuestionSame qi h = not $ getAnswer (Gt qi qi) h

-- ge always returns true if given two identical questions
propGeQuestionSame :: QInt -> PlayerHand -> Bool
propGeQuestionSame qi h = getAnswer (Ge qi qi) h

-- lt always returns false if given two identical questions
propLtQuestionSame :: QInt -> PlayerHand -> Bool
propLtQuestionSame qi h = not $ getAnswer (Lt qi qi) h

-- le always returns true if given two identical questions
propLeQuestionSame :: QInt -> PlayerHand -> Bool
propLeQuestionSame qi h = getAnswer (Le qi qi) h

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
                          (1, liftM2 IntersectionHand arbitrary arbitrary),
                          (5, liftM2 Filter arbitrary arbitrary) ]
  shrink (UnionHand qh1 qh2) = [qh1, qh2]
  shrink (IntersectionHand qh1 qh2) = [qh1, qh2]
  shrink (Filter _ qh) = [qh]
  shrink Hand = []
  
instance CoArbitrary Card where
  coarbitrary c = variant $ fromEnum (suit c) * 13 + fromEnum (rank c)

-------------------- GameState Tests --------------------
-- all cards distributed when game is initialized
propAllCardsDistributed :: Int -> Int -> Property
propAllCardsDistributed n a = n > a && a >= 0 ==> 
  let gs = initialGameStore n a
      allCards :: [Card]
      allCards = Prelude.foldr (\p acc -> Set.toList (hand p) ++ acc) [] (players gs) in
  length allCards == 52 && Set.size (Set.fromList allCards) == 52

-- cards evenly distributed when game is initialized
propCardsEvenDistributed :: Int -> Int -> Property
propCardsEvenDistributed n a = n > a && a >= 0 && n <= 52 ==> 
  let gs = initialGameStore n a
      maxSize = Prelude.foldr (\p acc -> max (Set.size (hand p)) acc) 0 (players gs)
      minSize = Prelude.foldr (\p acc -> min (Set.size (hand p)) acc) 52 (players gs) in
  maxSize == minSize || maxSize + 1 == minSize && 
  Prelude.foldr (\p acc -> (Set.size (hand p)) > 0 && (Set.size (hand p)) <= 52 && acc) True (players gs)

-- no face up cards when game initialized
propNoFaceUpOnStart :: Int -> Int -> Property
propNoFaceUpOnStart n a = n > a && a >= 0 ==> 
  let gs = initialGameStore n a in
  length (faceUpCards gs) == 0

unitTests :: Test
unitTests = TestList [
  testPlayerTurn ~?= True,
  testCheckWin ~?= True,
  testCheckTie ~?= True  ]

-- unit test for the right person's turn
testPlayerTurn :: Bool
testPlayerTurn = 
  let gs = initialGameStore 4 0
      firstPlayerId = pid ((players gs) !! 0)
      (ps2, gs2) = S.runState (move (players gs)) gs
      secondPlayerId = pid (ps2 !! 0) in
  firstPlayerId == 0 && secondPlayerId == 1

-- unit test for the end game where a player wins
testCheckWin :: Bool
testCheckWin = checkEnd winState

-- unit test for the end game where there is a tie
testCheckTie :: Bool
testCheckTie = checkEnd tieState

-- unit test to check that ranks are claimed correctly
testClaimRank :: Bool
testClaimRank =
  let gs = fakeGameAllCards
      gs2 = claimRank gs (players gs !! 0) Ace in
  (faceUpCards gs == []) && (size (hand (players gs !! 0)) == 52) &&
  (ranks (players gs2 !! 0) == Set.fromList [Ace]) && (size (hand (players gs2 !! 0)) == 48)

-- Helper functions to make writing test cases easier ---
winState :: GameStore
winState = 
  let n' = 4
      pids = [0..3]
      ranks = [Set.fromList [Ace ..Six], Set.fromList [Seven ..Queen], 
               Set.fromList [King], Set.empty]
      players = createPlayersWin pids ranks in
  G players []
  where
    createPlayersWin :: [Int] -> [Set Rank] -> [Player]
    createPlayersWin (id : ids) (r : ranks) = 
      (P id Set.empty r False) : createPlayersWin ids ranks
    createPlayersWin _ _ = [] 

tieState :: GameStore
tieState =
  let n' = 4
      pids = [0..3]
      ranks = [Set.fromList [Ace ..], Set.empty, Set.empty, Set.empty]
      players = createPlayersWin pids ranks in
  G players []
  where
    createPlayersWin :: [Int] -> [Set Rank] -> [Player]
    createPlayersWin (id : ids) (r : ranks) = 
      (P id Set.empty r False) : createPlayersWin ids ranks
    createPlayersWin _ _ = [] 

-- p1 has all diamonds, p2 has all clubs, p3 has all hearts, p4 has all spades
unshuffledGame :: GameStore
unshuffledGame =
  let n' = 4
      pids = [0..3]
      hands = dealDeckUnshuffled n' deck
      players = createPlayersUnshuffled pids (Prelude.take n' hands) in
  G players []
  where
    createPlayersUnshuffled :: [Int] -> [PlayerHand] -> [Player]
    createPlayersUnshuffled (id : ids) (h : hands) = 
      (P id h Set.empty False) : createPlayersUnshuffled ids hands
    createPlayersUnshuffled _ _ = []


-- | shuffle and deal deck to given number of players
dealDeckUnshuffled :: Int -> [Card] -> [PlayerHand]
dealDeckUnshuffled n cs = 
                let d = 52 `div` n
                    m = 52 `mod` n 
                    r = m * (d + 1) in
  if m > 0 
    then (deal (d + 1) (Prelude.take r cs)) ++ (deal d (Prelude.drop r cs))
  else deal d cs
  where
    deal _ [] = []
    deal n l = let (hd, tl) = Prelude.splitAt n l in
      (Set.fromList hd) : (deal n tl)


-- p1 has all cards
fakeGameAllCards :: GameStore
fakeGameAllCards =
  let n' = 4
      pids = [0..3]
      hands = [Set.fromList deck, Set.empty, Set.empty, Set.empty]
      players = createPlayersUnshuffled pids (Prelude.take n' hands) in
  G players []
  where
    createPlayersUnshuffled :: [Int] -> [PlayerHand] -> [Player]
    createPlayersUnshuffled (id : ids) (h : hands) = 
      (P id h Set.empty False) : createPlayersUnshuffled ids hands
    createPlayersUnshuffled _ _ = []


-- unit test for correct answer when player asks question
testCorrectAnswer :: Test
testCorrectAnswer = TestList []

-- unit test to check that ranks are claimed correctly
testLayOut :: Test
testLayOut = undefined