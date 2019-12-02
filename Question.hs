{-# OPTIONS -Wincomplete-patterns #-}
module Question where
import GameState
import Text.Read

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
--  deriving (Read)

instance Show Question where
  show (SpecificCard c) = "Do you have the " ++ show c ++ "?"
  show (NonEmpty qh) = "Is the set {" ++ show qh ++ "} empty?"
  show (Union q1 q2) = "(" ++ show q1 ++ ") or (" ++ show q2 ++ ")?"
  show (Intersection q1 q2) = "(" ++ show q1 ++ ") and (" ++ show q2 ++ ")?"
  show (Not q) = "Not (" ++ show q ++ ")"
  show (Equals qi1 qi2) = "Is (" ++ show qi1 ++ ") equal to (" ++ show qi2 ++ ")?"
  show (Gt qi1 qi2) = "Is (" ++ show qi1 ++ ") greater than (" ++ show qi2 ++ ")?"
  show (Lt qi1 qi2) = "Is (" ++ show qi1 ++ ") less than (" ++ show qi2 ++ ")?"
  show (Ge qi1 qi2) = "Is (" ++ show qi1 ++ ") greater than or equal to (" ++ show qi2 ++ ")?"
  show (Le qi1 qi2) = "Is (" ++ show qi1 ++ ") less than or equal to (" ++ show qi2 ++ ")?"

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
--  deriving (Read)

instance Show QInt where
  show (IntVal i) = show i
  show (Cardinality qh) = "the size of {" ++ show qh ++ "}"
  show (SumHand qh) = "the sum of {" ++ show qh ++ "}"
  show (ProductHand qh) = "the product of {" ++ show qh ++ "}"
  show (Sum qi1 qi2) = "(" ++ show qi1 ++ ") plus (" ++ show qi2 ++ ")" 
  show (Diff qi1 qi2) = "(" ++ show qi1 ++ ") minus (" ++ show qi2 ++ ")" 
  show (Mod qi1 qi2) = "(" ++ show qi1 ++ ") mod (" ++ show qi2 ++ ")" 
  show (Product qi1 qi2) = "(" ++ show qi1 ++ ") times (" ++ show qi2 ++ ")" 
  show (Quotient qi1 qi2) = "(" ++ show qi1 ++ ") divided by (" ++ show qi2 ++ ")" 


data QHand = 
  Hand                             -- -> hand
  | Filter (Card -> Bool) QHand    -- (Card -> Bool) -> ((hand)) -> hand
  | UnionHand QHand QHand          -- ((hand)) -> ((hand)) -> hand
  | IntersectionHand QHand QHand   -- ((hand)) -> ((hand)) -> hand
-- deriving (Read)
  
instance Show QHand where
  show Hand = "Hand"
  show (UnionHand qh1 qh2) = "UnionHand(" ++ show qh1 ++ ", " ++ show qh2 ++ ")"
  show (IntersectionHand qh1 qh2) = "IntersectionHand(" ++ show qh1 ++ ", " ++ show qh2 ++ ")"
  show (Filter f qh) = "Filter([" ++ (findTrueCard f) ++ "], " ++ show qh ++ ")"
    where findTrueCard :: (Card -> Bool) -> String
          findTrueCard f = foldr (\x acc -> if f x then show x else acc) "N/A" deck

-- instance Read QHand where
--   readsPrec "Hand" = Hand
--   readsPrec _ = undefined

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

questionParser :: String -> Question
questionParser = undefined