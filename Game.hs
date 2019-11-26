{-# OPTIONS -Wincomplete-patterns #-}
module Game where
import qualified State as S
import qualified Data.Maybe as Maybe
import Question
import GameState

-------------------------------------------------------------------------------

-- | displays game statistics once the game ends
displayEnd :: Game -> IO ()
displayEnd = undefined

-- | query the current player for a question about another player
getQuestion :: Game -> Player -> IO Question
getQuestion = undefined

-- | answers player's question and displays answer
displayAnswer :: Question -> Player -> IO Bool
displayAnswer = undefined

-- | make moves until someone wins
-- TODO: restrict arguments
main :: Int -> Int -> IO ()
main numPlayers numAI = let initialStore = initialGameStore numPlayers numAI
                            sequence = players initialStore in
  go sequence initialStore where
    go :: [Player] -> GameStore -> IO()
    go sequence store = let (sequence', store') = S.runState (move sequence) store in do
        putStrLn (show (sequence !! 0))
        str <- getLine
        go sequence' store'

move :: [Player] -> Game
move [] = return []
move (x:xs) = do
  return (xs ++ [x])
