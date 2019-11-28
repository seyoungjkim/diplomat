{-# OPTIONS -Wincomplete-patterns #-}
module Game where
import qualified State as S
import qualified Data.Maybe as Maybe
import Text.Read
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
    go sequence store = 
      let (sequence', store') = S.runState (move sequence) store in do
        let player = sequence !! 0
        let pids = foldr (\p acc -> (pid p) : acc) [] (players store)
        putStrLn ("Your hand:")
        putStrLn (show $ hand player)
        putStr ("Player " ++ (show $ pid player) ++ ": Enter a player id> ")
        playerToQuestion <- getLine
        case (readMaybe playerToQuestion :: Maybe Int) of
          Just i -> if elem i pids then do
                      putStr ("Player " ++ (show $ pid player) ++ ": Enter a question> ")
                      q <- getLine
                      case q of
                        "q" -> return () -- quit the game
                        "none" -> go sequence' store' -- skip turn
                        _   -> putStrLn "invalid question" >> go sequence store -- unknown command
                    else putStrLn "please enter a valid player id" >> go sequence store
          Nothing -> putStrLn "please enter a player id" >> go sequence store
        
          
        

move :: [Player] -> Game
move [] = return []
move (x:xs) = do
  return (xs ++ [x])
