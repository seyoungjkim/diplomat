import State (State)
import qualified State as S

import qualified Data.Maybe as Maybe

main :: IO ()
main = return ()

-------------------------------------------------------------------------

data Card = Card { rank :: Int, suit :: String }
  deriving (Eq, Show, Ord)

data Question = 
  CardMod
  | CardSum
  | CardProd
  deriving (Eq, Show)
  
{-|

PROPERTIES:
- mod
- sum
- prduct

- set operation

-}


-- “Do you have the queen of spades?”, 
-- “Do you have at least three black cards?”, 
-- “Is the sum of the ranks of your non-face cards at least 50?”)

-- aggregating cards in your hand: “Is the sum of the ranks in your hand…”; 
-- having a specific subset of cards: “Do you have any Red 5s, Black 10s, or Black Queens?”; 
-- parity of cards in your hand: “Do you have any even cards?”

-- All questions in “Diplomat” must be “equivalent to specifying a set S of possible 
-- hands and asking whether the player’s hand lies in S” 
-- we can do interesting operations on these sets (i.e. union, intersection, etc…).
