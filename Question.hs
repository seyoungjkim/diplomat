{-# OPTIONS -Wincomplete-patterns #-}
module Question where
import GameState

import qualified State as S
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

-------------------------------------------------------------------------
data Question = 
  SpecificCard Card                -- -> bool
  | NonEmpty QHand                 -- ((hand)) -> bool
  | Union Question Question        -- ((bool)) -> ((bool)) -> bool
  | Intersection Question Question -- ((bool)) -> ((bool)) -> bool
  | Not Question                   -- ((bool)) -> bool
  | Equals QInt QInt               -- ((int)) -> ((int)) -> bool
  | Gt QInt QInt                   -- ((int)) -> ((int)) -> bool
  | Lt QInt QInt                   -- ((int)) -> ((int)) -> bool
  | Ge QInt QInt                   -- ((int)) -> ((int)) -> bool
  | Le QInt QInt                   -- ((int)) -> ((int)) -> bool
  deriving (Show)

data QInt = 
  IntVal Int                       -- -> int
  | Cardinality QHand              -- ((hand)) -> int
  | SumHand QHand                  -- ((hand)) -> int
  | ProductHand QHand              -- ((hand)) -> int
  | Sum QInt QInt                  -- ((int)) -> ((int)) -> int
  | Diff QInt QInt                 -- ((int)) -> ((int)) -> int
  | Mod QInt QInt                  -- ((int)) -> ((int)) -> int
  | Product QInt QInt              -- ((int)) -> ((int)) -> int
  | Quotient QInt QInt             -- ((int)) -> ((int)) -> int
  deriving (Show)

data QHand = 
  Hand                             -- -> hand
  | Filter (Card -> Bool) QHand    -- (Card -> Bool) -> ((hand)) -> hand
  | UnionHand QHand QHand          -- ((hand)) -> ((hand)) -> hand
  | IntersectionHand QHand QHand   -- ((hand)) -> ((hand)) -> hand
  
instance Show QHand where
  show (Hand) = "Hand"
  show (UnionHand qh1 qh2) = "UnionHand" ++ show qh1 ++ show qh2
  show (IntersectionHand qh1 qh2) = "IntersectionHand" ++ show qh1 ++ show qh2
  show _ = show "hi" 

-------------------------------------------------------------------------
-- | returns bool value of a top-level question
getAnswer :: Question -> PlayerHand -> Bool
getAnswer (SpecificCard c) h      = Set.member c h
getAnswer (NonEmpty qh) h         = not $ null (getAnswerHand qh h)
getAnswer (Union q1 q2) h         = getAnswer q1 h || getAnswer q2 h
getAnswer (Intersection q1 q2) h  = getAnswer q1 h && getAnswer q2 h
getAnswer (Not q) h               = not $ getAnswer q h
getAnswer (Equals qi1 qi2) h      = getAnswerInt qi1 h == getAnswerInt qi2 h
getAnswer (Gt qi1 qi2) h          = getAnswerInt qi1 h > getAnswerInt qi2 h
getAnswer (Lt qi1 qi2) h          = getAnswerInt qi1 h < getAnswerInt qi2 h
getAnswer (Ge qi1 qi2) h          = getAnswerInt qi1 h >= getAnswerInt qi2 h
getAnswer (Le qi1 qi2) h          = getAnswerInt qi1 h <= getAnswerInt qi2 h
  
-- | returns int value of a question
getAnswerInt :: QInt -> PlayerHand -> Int
getAnswerInt (IntVal i) h         = i
getAnswerInt (Cardinality qh) h   = length (getAnswerHand qh h)
getAnswerInt (SumHand qh) h       = foldr plusCard 0 (getAnswerHand qh h)
getAnswerInt (ProductHand qh) h   = foldr multiplyCard 1 (getAnswerHand qh h)
getAnswerInt (Sum qi1 qi2) h      = getAnswerInt qi1 h + getAnswerInt qi2 h
getAnswerInt (Diff qi1 qi2) h     = getAnswerInt qi1 h - getAnswerInt qi2 h
getAnswerInt (Mod qi1 qi2) h      = case (getAnswerInt qi2 h) of
  0 -> 0
  x -> getAnswerInt qi1 h `mod` x
getAnswerInt (Product qi1 qi2) h  = getAnswerInt qi1 h * getAnswerInt qi2 h
getAnswerInt (Quotient qi1 qi2) h = case (getAnswerInt qi2 h) of
  0 -> 0
  x -> getAnswerInt qi1 h `div` x
  
-- | returns hand value for a question
getAnswerHand :: QHand -> PlayerHand -> PlayerHand
getAnswerHand Hand h = h
getAnswerHand (UnionHand qh1 qh2) h = 
  Set.union (getAnswerHand qh1 h) (getAnswerHand qh2 h)
getAnswerHand (IntersectionHand qh1 qh2) h = 
  Set.intersection (getAnswerHand qh1 h) (getAnswerHand qh2 h)
getAnswerHand (Filter f qh) h = 
  Set.filter f (getAnswerHand qh h)
