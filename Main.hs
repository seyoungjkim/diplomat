module Main where
import Game (play)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Random (randomIO)

main :: IO ()
main = do
  seed <- randomIO :: IO Int
  args <- getArgs
  case args of
    [numPlayers, numAi] -> 
      case (readMaybe numPlayers :: Maybe Int, readMaybe numAi :: Maybe Int) of
        (Just np, Just na) -> play seed np na
        _ -> putStrLn "Please enter a valid number of human and AI players."
    _ -> putStrLn "Please enter a valid number of human and AI players."
