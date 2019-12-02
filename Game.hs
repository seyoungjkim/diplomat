{-# OPTIONS -Wincomplete-patterns #-}
module Game where
import qualified State as S
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.List as List
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
    putStrLn ("Laid out cards:")
    putStrLn (show $ faceUpCards store)
    putStrLn ("Your hand:")
    putStrLn (show $ hand player)
    putStrLn ("Do you want to claim any rank? Please enter y or n:")
    rankToClaim <- getLine
    if (rankToClaim == "y") then claimRankIO store player
    else putStrLn ("Ok, please ask a player a question now.")
    putStr ("Player " ++ (show $ pid player) ++ ": Enter a player id> ")
    playerToQuestion <- getLine
    case (readMaybe playerToQuestion :: Maybe Int) of
      Just i -> if elem i pids then
                  let player' = sequence !! i in do
                    -- TODO: how to get the right player since seq changes?
                      -- idea: maybe each player sees the players clockwise from 
                      -- him as 1,2,3 since their relative order won't change?
                    putStrLn ("Player " ++ (show $ pid player) ++ ": Enter a question> ")
                    putStrLn questionOptions
                    q <- getLine
                    case q of
                      "1" -> questionSpecificCard player' sequence store >> go sequence' store'
                      "q" -> return () -- quit the game
                      "none" -> go sequence' store' -- skip turn
                      _   -> putStrLn "invalid question" >> go sequence store -- unknown command
                else putStrLn "please enter a valid player id" >> go sequence store
      Nothing -> putStrLn "please enter a player id" >> go sequence store

questionSpecificCard :: Player -> [Player] -> GameStore -> IO ()
questionSpecificCard player sequence store = do
  putStrLn "Enter a suit: Diamond, Club, Heart, or Spade."
  suit <- getLine
  putStrLn "Enter a rank."
  rank <- getLine
  case (readMaybe suit :: Maybe Suit, readMaybe rank :: Maybe Rank) of
    (Just s, Just r) ->
      let c = (Card r s)
          q = SpecificCard c
          a =  getAnswer q (hand player) in
        putStrLn (show (q :: Question)) >>
        putBool a >>
        putStrLn "========================================"
        -- case a of
        --   True -> let gs' = layoutCard store player c in
        --           go (sequence gs) gs'
        --   _ -> putStrLn ("No, they didn't have " ++ show c)
    _ -> putStrLn "invalid suit or rank" >> go sequence store

-- layoutCard :: GameStore -> Player -> Card -> GameStore
-- layoutCard gs p c = let newPlayerHand = Set.delete c (hand player)
--                         newFaceUp = c : (faceUpCards store)
--                         allNewPlayers = (P (pid p) newPlayerHand (ranks p) (ai p)) : (List.delete p (players gs)) in
--   G allNewPlayers newFaceUp

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
  -- I think they can create filters for each individual suit or rank for now?
  -- And then they can just combine them themselves?
questionHandOptions :: String
questionHandOptions = "1: Hand \n \
                      \2: Filter \n \
                      \3: UnionHand \n \
                      \4: IntersectionHand \n \
                      \done: finish the question \n \
                      \none: skip your turn"

claimRankIO :: GameStore -> Player -> IO ()
claimRankIO gs p = do
  putStrLn ("Please enter a valid rank (1-13):")
  rank <- getLine
  case (readMaybe rank :: Maybe Rank) of
    Just r -> 
      let gs' = claimRank gs p r in
      go (players gs) gs'
    _ -> putStrLn ("Please enter a valid rank (1-13):") >>
         claimRankIO gs p

claimRank :: GameStore -> Player -> Rank -> GameStore
claimRank gs p r = let playerRanks = filter (\c -> rank c == r) (Set.toList $ hand p)
                       laidOutRanks = filter (\c -> rank c == r) (faceUpCards gs) in
  if (length playerRanks + length laidOutRanks == 4) then
    let newPlayerHand = foldr (\c acc -> Set.delete c acc) (hand p) playerRanks
        newLaidOut = foldr (\c acc -> List.delete c acc) (faceUpCards gs) laidOutRanks
        playerClaimedRanks = Set.insert r (ranks p)
        allNewPlayers = (P (pid p) newPlayerHand playerClaimedRanks (ai p)) : (List.delete p (players gs)) in
    G allNewPlayers newLaidOut
  else gs       

move :: [Player] -> Game
move [] = return []
move (x:xs) = do
  return (xs ++ [x])

-- questionBuilder :: Player -> [Player] -> GameStore -> IO()
-- questionBuilder player sequence store = do
--   putStrLn questionOptions
--   q <- getLine
--   case q of
--     "1" -> questionSpecificCard player sequence store -- specific card
--     "2" -> NotEmpty () -- non-empty
--     "q" -> return () -- quit the game
--     "none" -> go sequence' store' -- skip turn
--     _   -> putStrLn "invalid question" >> go sequence store -- unknown command

-- --questionHandBuilder :: GameStore -> IO()