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
  go sequence initialStore

go :: [Player] -> GameStore -> IO ()
go sequence store = 
  let (sequence', store') = S.runState (move sequence) store in do
    let player = sequence !! 0
    let pids = foldr (\p acc -> (pid p) : acc) [] (players store)
    putStrLn ("Your hand:")
    putStrLn (show $ hand player)
    putStr ("Player " ++ (show $ pid player) ++ ": Enter a player id> ")
    playerToQuestion <- getLine
    case (readMaybe playerToQuestion :: Maybe Int) of
      Just i -> if elem i pids then
                  let player' = sequence !! i in do
                    -- TODO: how to get the right player since seq changes?
                    putStrLn questionOptions
                    putStrLn ("Player " ++ (show $ pid player) ++ ": Enter a question> ")
                    q <- getLine
                    case q of
                      "1" -> questionCard player' sequence store
                      "q" -> return () -- quit the game
                      "none" -> go sequence' store' -- skip turn
                      _   -> putStrLn "invalid question" >> go sequence store -- unknown command
                else putStrLn "please enter a valid player id" >> go sequence store
      Nothing -> putStrLn "please enter a player id" >> go sequence store

questionCard :: Player -> [Player] -> GameStore -> IO ()
questionCard player sequence store = do
  putStrLn "Enter a suit. 0: Diamond, 1: Club, 2: Heart, 3: Spade"
  suit <- getLine
  putStrLn "Enter a rank."
  rank <- getLine
  case (readMaybe suit, readMaybe rank) of
    (Just s, Just r) ->
      let s' = toEnum s
          r' = toEnum (r-1) :: Rank
          q = SpecificCard (Card r' s')
          a = getAnswer q (hand player) in
        putStrLn (show (q :: Question)) >>
        putBool a >>
        go sequence store
    _ -> putStrLn "invalid suit or rank" >> go sequence store

putBool :: Bool -> IO ()
putBool True = putStrLn "Yes"
putBool False = putStrLn "No"
          
questionOptions :: String  
questionOptions = "1: Specific card \n \
                  \2: Non-empty \n \
                  \3: Union \n \
                  \4: Intersection \n \
                  \5: Not \n \
                  \6: Equals \n \
                  \7: Greater than \n \
                  \8: Less than \n \
                  \9: Greater than or equal to \n \
                  \10: Greater than or equal to \n \
                  \done: finish the question \n \
                  \none: skip your turn"

questionIntOptions :: String  
questionIntOptions = "1: IntVal \n \
                     \2: Cardinality \n \
                     \3: SumHand \n \
                     \4: ProductHand \n \
                     \5: Sum \n \
                     \6: Diff \n \
                     \7: Mod \n \
                     \8: Product \n \
                     \done: finish the question \n \
                     \none: skip your turn"

-- TODO: how to have user input filter?
questionHandOptions :: String
questionHandOptions = "1: Hand \n \
                      \2: Filter \n \
                      \3: UnionHand \n \
                      \4: IntersectionHand \n \
                      \done: finish the question \n \
                      \none: skip your turn"

move :: [Player] -> Game
move [] = return []
move (x:xs) = do
  return (xs ++ [x])
