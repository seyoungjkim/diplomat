{-# OPTIONS -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Werror #-}

module Question where

import GamePieces (Card, PlayerHand, plusCard, multiplyCard, deck)

import Text.Read (readMaybe)
import qualified State as S
import qualified Data.Set as Set (member, empty, union, intersection, filter)
import Data.Maybe as Maybe ()


-- Component of a question which results in a bool
-- As the overall question must result in a bool, 
--    this is the parent question type.
data Question = 
  Blank
  | SpecificCard Card              -- -> bool
  | NonEmpty QHand                 -- ((hand)) -> bool
  | Union Question Question        -- ((bool)) -> ((bool)) -> bool
  | Intersection Question Question -- ((bool)) -> ((bool)) -> bool
  | Not Question                   -- ((bool)) -> bool
  | Equals QInt QInt               -- ((int)) -> ((int)) -> bool
  | Gt QInt QInt                   -- ((int)) -> ((int)) -> bool
  | Lt QInt QInt                   -- ((int)) -> ((int)) -> bool
  | Ge QInt QInt                   -- ((int)) -> ((int)) -> bool
  | Le QInt QInt                   -- ((int)) -> ((int)) -> bool

instance Show Question where
  show Blank = "_"
  show (SpecificCard c) = "Do you have the " ++ show c ++ "?"
  show (NonEmpty qh) = "Is there anything in the hand {" ++ show qh ++ "}?"
  show (Union q1 q2) = "(" ++ show q1 ++ ") or (" ++ show q2 ++ ")?"
  show (Intersection q1 q2) = "(" ++ show q1 ++ ") and (" ++ show q2 ++ ")?"
  show (Not q) = "Not (" ++ show q ++ ")"
  show (Equals qi1 qi2) = 
    "Is (" ++ show qi1 ++ ") equal to (" ++ show qi2 ++ ")?"
  show (Gt qi1 qi2) = 
    "Is (" ++ show qi1 ++ ") greater than (" ++ show qi2 ++ ")?"
  show (Lt qi1 qi2) = "Is (" ++ show qi1 ++ ") less than (" ++ show qi2 ++ ")?"
  show (Ge qi1 qi2) = 
    "Is (" ++ show qi1 ++ ") greater than or equal to (" ++ show qi2 ++ ")?"
  show (Le qi1 qi2) = 
    "Is (" ++ show qi1 ++ ") less than or equal to (" ++ show qi2 ++ ")?"

-- Component of a question which results in an int 
data QInt = 
  BlankQInt
  | IntVal Int                     -- -> int
  | Cardinality QHand              -- ((hand)) -> int
  | SumHand QHand                  -- ((hand)) -> int
  | ProductHand QHand              -- ((hand)) -> int
  | Sum QInt QInt                  -- ((int)) -> ((int)) -> int
  | Diff QInt QInt                 -- ((int)) -> ((int)) -> int
  | Mod QInt QInt                  -- ((int)) -> ((int)) -> int
  | Product QInt QInt              -- ((int)) -> ((int)) -> int
  | Quotient QInt QInt             -- ((int)) -> ((int)) -> int

instance Show QInt where
  show BlankQInt = "_"
  show (IntVal i) = show i
  show (Cardinality qh) = "the size of {" ++ show qh ++ "}"
  show (SumHand qh) = "the sum of {" ++ show qh ++ "}"
  show (ProductHand qh) = "the product of {" ++ show qh ++ "}"
  show (Sum qi1 qi2) = "(" ++ show qi1 ++ ") plus (" ++ show qi2 ++ ")" 
  show (Diff qi1 qi2) = "(" ++ show qi1 ++ ") minus (" ++ show qi2 ++ ")" 
  show (Mod qi1 qi2) = "(" ++ show qi1 ++ ") mod (" ++ show qi2 ++ ")" 
  show (Product qi1 qi2) = "(" ++ show qi1 ++ ") times (" ++ show qi2 ++ ")" 
  show (Quotient qi1 qi2) = 
    "(" ++ show qi1 ++ ") divided by (" ++ show qi2 ++ ")" 

-- Component of a question which results in a hand 
data QHand = 
  BlankQHand
  | Hand                           -- -> hand
  | Filter (Card -> Bool) QHand    -- (Card -> Bool) -> ((hand)) -> hand
  | UnionHand QHand QHand          -- ((hand)) -> ((hand)) -> hand
  | IntersectionHand QHand QHand   -- ((hand)) -> ((hand)) -> hand
  
instance Show QHand where
  show BlankQHand = "_"
  show Hand = "Hand"
  show (UnionHand qh1 qh2) = "UnionHand(" ++ show qh1 ++ ", " ++ show qh2 ++ ")"
  show (IntersectionHand qh1 qh2) = 
    "IntersectionHand(" ++ show qh1 ++ ", " ++ show qh2 ++ ")"
  show (Filter f qh) = 
    "Filtering ([ex. " ++ findFalseCard f ++ "], " ++ show qh ++ ")"
    where findFalseCard :: (Card -> Bool) -> String
          findFalseCard f = 
            foldr (\x acc -> if not (f x) then show x else acc) "N/A" deck

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
getAnswer Blank _                 = False
  
-- | returns int value of a question
getAnswerInt :: QInt -> PlayerHand -> Int
getAnswerInt (IntVal i) h         = i
getAnswerInt (Cardinality qh) h   = length (getAnswerHand qh h)
getAnswerInt (SumHand qh) h       = foldr plusCard 0 (getAnswerHand qh h)
getAnswerInt (ProductHand qh) h   = foldr multiplyCard 1 (getAnswerHand qh h)
getAnswerInt (Sum qi1 qi2) h      = getAnswerInt qi1 h + getAnswerInt qi2 h
getAnswerInt (Diff qi1 qi2) h     = getAnswerInt qi1 h - getAnswerInt qi2 h
getAnswerInt (Mod qi1 qi2) h      = case getAnswerInt qi2 h of
  0 -> 0
  x -> getAnswerInt qi1 h `mod` x
getAnswerInt (Product qi1 qi2) h  = getAnswerInt qi1 h * getAnswerInt qi2 h
getAnswerInt (Quotient qi1 qi2) h = case getAnswerInt qi2 h of
  0 -> 0
  x -> getAnswerInt qi1 h `div` x
getAnswerInt BlankQInt _          = 0
  
-- | returns hand value for a question
getAnswerHand :: QHand -> PlayerHand -> PlayerHand
getAnswerHand Hand h = h
getAnswerHand (UnionHand qh1 qh2) h = 
  Set.union (getAnswerHand qh1 h) (getAnswerHand qh2 h)
getAnswerHand (IntersectionHand qh1 qh2) h = 
  Set.intersection (getAnswerHand qh1 h) (getAnswerHand qh2 h)
getAnswerHand (Filter f qh) h = 
  Set.filter f (getAnswerHand qh h)
getAnswerHand BlankQHand _ = Set.empty

------------------------ Question Options for Building ------------------------
-- | Prompts user for type of question to ask
questionOptionsInitial :: String  
questionOptionsInitial = 
  " 1: Ask for a specific card \n\
  \ 2: Build a more complex question \n \
  \none: skip your turn"

-- | Current built-up Question
questionOptionsBuilding :: Question ->  String  
questionOptionsBuilding q = 
  "\n>> So far your question is " ++ show q ++ ". Choose one:  \n \
  \1: Non-empty (hand) \n \
  \2: Union (question) (question) \n \
  \3: Intersection (question) (question) \n \
  \4: Not (question) \n \
  \5: Equals (int) (int) \n \
  \6: Greater than (int) (int) \n \
  \7: Less than (int) (int) \n \
  \8: Greater than or equal to (int) (int) \n \
  \9: Less than or equal to (int) (int)"

-- | Builds up Question from given String
readQuestionOptionsBuilding :: String -> Maybe Question
readQuestionOptionsBuilding s = 
  case readMaybe s :: Maybe Int of
    Just 1 -> Just $ NonEmpty BlankQHand
    Just 2 -> Just $ Union Blank Blank
    Just 3 -> Just $ Intersection Blank Blank
    Just 4 -> Just $ Not Blank
    Just 5 -> Just $ Equals BlankQInt BlankQInt
    Just 6 -> Just $ Gt BlankQInt BlankQInt
    Just 7 -> Just $ Lt BlankQInt BlankQInt
    Just 8 -> Just $ Ge BlankQInt BlankQInt
    Just 9 -> Just $ Le BlankQInt BlankQInt
    _ -> Nothing
     
-- | Current built-up Question
-- which needs to take in a QInt next
questionIntOptions :: Question -> String  
questionIntOptions q = 
  "\n>> So far your question is " ++ show q ++ ". Choose one:  \n \
   \1: Integer \n \
   \2: Cardinality (hand) \n \
   \3: SumHand (hand) \n \
   \4: ProductHand (hand) \n \
   \5: Sum (int) (int) \n \
   \6: Diff (int) (int) \n \
   \7: Mod (int) (int) \n \
   \8: Product (int) (int) \n \
   \9: Quotient (int) (int)"

-- | Builds up QInt from given String
readQuestionIntOptions :: String -> Maybe QInt
readQuestionIntOptions s = 
  case readMaybe s :: Maybe Int of
    Just 1 -> Just $ IntVal 0
    Just 2 -> Just $ Cardinality BlankQHand
    Just 3 -> Just $ SumHand BlankQHand
    Just 4 -> Just $ ProductHand BlankQHand
    Just 5 -> Just $ Sum BlankQInt BlankQInt
    Just 6 -> Just $ Diff BlankQInt BlankQInt
    Just 7 -> Just $ Mod BlankQInt BlankQInt
    Just 8 -> Just $ Product BlankQInt BlankQInt
    Just 9 -> Just $ Quotient BlankQInt BlankQInt
    _ -> Nothing

-- | Current built-up Question
-- which needs to take in a QHand next
questionHandOptions :: Question -> String
questionHandOptions q = 
  "\n>> So far your question is " ++ show q ++ ". Choose one:  \n \
  \1: Player's hand \n \
  \2: Filter (filter function) (hand) \n \
  \3: UnionHand (hand) (hand) \n \
  \4: IntersectionHand (hand) (hand)"

-- | Builds up QHand from given String
readQuestionHandOptions :: String -> Maybe QHand
readQuestionHandOptions s = 
  case readMaybe s :: Maybe Int of
    Just 1 -> Just Hand
    Just 2 -> Just $ Filter (const True) BlankQHand
    Just 3 -> Just $ UnionHand BlankQHand BlankQHand
    Just 4 -> Just $ IntersectionHand BlankQHand BlankQHand
    _ -> Nothing