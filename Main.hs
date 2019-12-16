module Main where
import Game
import System.Environment
import Text.Read

main :: IO ()
main = do 
  args <- getArgs
  case args of
    (numPlayers : numAi : []) -> 
      case (readMaybe numPlayers :: Maybe Int, readMaybe numAi :: Maybe Int) of
        (Just np, Just na) -> Game.play np na
        _ -> putStrLn "Please enter a valid number of human and AI players."
    _ -> putStrLn "Please enter a valid number of human and AI players."