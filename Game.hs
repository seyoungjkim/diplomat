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
main :: Game -> IO ()
main = undefined

move :: Game -> GameStore -> IO ()
move g gs = do
    putStrLn $ "player: " ++ show x ++ ", gamestore: " ++ show s
    where (x,s) = S.runState g gs