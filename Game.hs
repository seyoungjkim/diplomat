{-# OPTIONS -Wincomplete-patterns #-}
module Game where

import qualified State as S
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Map as Map
import Text.Read
import Question
import BuildQuestion
import GamePieces

-- Game represented as a GameStore and current player
type Game = S.State GameStore [Int]

data GameStore = G { players :: Map Int Player, faceUpCards :: [Card], currQuestion :: Question }

instance Show GameStore where
  show = undefined

-------------------------------------------------------------------------------

-- | initializes game given the number of total players and AIs
initialGameStore :: Int -> Int -> GameStore
initialGameStore n a =
  let n' = n - a
      pids = [0..n' - 1]
      aids = [n'..n - 1]
      hands = dealDeck n deck
      players = createPlayers pids (Prelude.take n' hands) False
      ais = createPlayers aids (Prelude.drop n' hands) True in
  G (Map.union players ais) [] Blank
  where
    createPlayers :: [Int] -> [PlayerHand] -> Bool -> Map Int Player
    createPlayers (id : ids) (h : hands) b = 
      Map.insert id (P id h Set.empty b) (createPlayers ids hands b)
    createPlayers _ _ _ = Map.empty
    createCycle :: [Player] -> [Player]
    createCycle ps = ps ++ createCycle ps

-- | checks if any player has won the game
checkEnd :: GameStore -> Bool
checkEnd gs = check [Ace ..] (players gs) where
  check :: [Rank] -> Map Int Player -> Bool
  check [] _ = True
  check (r:rs) players =
    Map.foldr (\p acc -> Set.member r (ranks p) || acc) False players && check rs players

-- | displays game statistics once the game ends
displayEnd :: Game -> IO ()
displayEnd = undefined

-- | make moves until someone wins
-- TODO: restrict arguments
main :: Int -> Int -> IO ()
main numPlayers numAI = let initialStore = initialGameStore numPlayers numAI
                            sequence = [0..numPlayers + numAI] in
  goIntro sequence initialStore

goIntro :: [Int] -> GameStore -> IO ()
goIntro sequence store = 
  let player = (players store) ! (sequence !! 0) in do
    putStrLn ("\nHi Player " ++ (show $ pid player) ++ "!! :)")
    putStrLn ("Curr claimed ranks:")
    putStrLn (show $ (players store))
    putStrLn ("Laid out cards:")
    putStrLn (show $ faceUpCards store)
    putStrLn ("Your current claimed ranks:")
    putStrLn (show $ Set.toList (ranks player))
    putStrLn ("Your hand:")
    putStrLn (show $ hand player)
    goClaim sequence store

goClaim :: [Int] -> GameStore -> IO ()
goClaim sequence store =
  let player = (players store) ! (sequence !! 0) in do
    claimRankIO store player sequence
    goQuestion sequence store

goQuestion :: [Int] -> GameStore -> IO ()
goQuestion sequence store = 
  let (sequence', _) = S.runState (move sequence) store in do
    let player = (players store) ! (sequence !! 0)
    putStrLn ("Ok, please ask a player a question now.")
    putStr ("Player " ++ (show $ pid player) ++ ": Enter a player id> ")
    playerIdToQuestion <- getLine
    case (readMaybe playerIdToQuestion :: Maybe Int) of
      Just i -> case Map.lookup i (players store) of
        Just playerToQuestion -> do
          putStrLn ("Player " ++ (show $ pid player) ++ ": Enter a question> ")
          putStrLn questionOptions
          q <- getLine
          case q of
            "1" -> questionSpecificCard playerToQuestion sequence store
            "q" -> return () -- quit the game
            "none" -> goIntro sequence' store -- skip turn
            _   -> putStrLn "invalid question" >> goQuestion sequence store -- unknown command
        Nothing -> putStrLn "please enter a valid player id" >> goQuestion sequence store
      Nothing -> putStrLn "please enter an integer player id" >> goQuestion sequence store

questionSpecificCard :: Player -> [Int] -> GameStore -> IO ()
questionSpecificCard player sequence store = do
  putStrLn "Enter a rank."
  rank <- getLine
  putStrLn "Enter a suit: Diamond, Club, Heart, or Spade."
  suit <- getLine
  case (readMaybe suit :: Maybe Suit, readMaybe rank :: Maybe Rank) of
    (Just s, Just r) ->
      let c = (Card r s)
          q = SpecificCard c
          a =  getAnswer q (hand player) in
        putStrLn (show (q :: Question)) >>
        case a of
          True -> let gs' = layoutCard store player c in
                  putStrLn ("Yes, they had " ++ show c) >> goClaim sequence gs'
          _ -> putStrLn ("No, they didn't have " ++ show c)
    _ -> putStrLn "invalid suit or rank" >> goQuestion sequence store

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

questionOptions2 :: Question ->  String  
questionOptions2 q = "So far your question is " ++ show q ++ ". Choose one:  \n \
                  \1: Non-empty (hand) \n \
                  \2: Union (question) (question) \n \
                  \3: Intersection (question) (question) \n \
                  \4: Not (question) \n \
                  \5: Equals (int) (int) \n \
                  \6: Greater than (int) (int) \n \
                  \7: Less than (int) (int) \n \
                  \8: Greater than or equal to (int) (int) \n \
                  \9: Less than or equal to (int) (int)"
                  

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

claimRankIO :: GameStore -> Player -> [Int] -> IO ()
claimRankIO gs p sequence = do
  putStrLn "Do you want to claim a rank?"
  claimYN <- getLine
  if (claimYN == "y") then do
    putStrLn "Please enter a valid rank:"
    rank <- getLine
    case (readMaybe rank :: Maybe Rank) of
      Just r -> 
        let gs' = claimRank gs p r in
        goClaim sequence gs'
      _ -> putStrLn "Please enter a valid rank:" >>
          claimRankIO gs p sequence
  else putStr "" -- this is dumb

-- THIS IS BROKEN!!!! not getting put into player ranks :(
-- need to add io message about claiming ranks
claimRank :: GameStore -> Player -> Rank -> GameStore
claimRank gs p r = let playerRanks = Prelude.filter (\c -> rank c == r) (Set.toList $ hand p)
                       laidOutRanks = Prelude.filter (\c -> rank c == r) (faceUpCards gs) in
  if (length playerRanks + length laidOutRanks == 4) then
    let newPlayerHand = Prelude.foldr (\c acc -> Set.delete c acc) (hand p) playerRanks
        newLaidOut = Prelude.foldr (\c acc -> List.delete c acc) (faceUpCards gs) laidOutRanks
        playerClaimedRanks = Set.insert r (ranks p)
        updatedPlayer = P (pid p) newPlayerHand playerClaimedRanks (ai p)
        newPlayerMap = Map.insert (pid p) updatedPlayer (players gs) in
    G newPlayerMap newLaidOut Blank       
  else gs   

-- layoutCardIO :: GameStore -> Player -> Card -> IO ()
-- layoutCardIO gs p c = undefined

layoutCard :: GameStore -> Player -> Card -> GameStore
layoutCard gs p c = let newPlayerHand = Set.delete c (hand p)
                        newFaceUp = c : (faceUpCards gs)
                        updatedPlayer = P (pid p) newPlayerHand (ranks p) (ai p)
                        newPlayerMap = Map.insert (pid p) updatedPlayer (players gs) in
  G newPlayerMap newFaceUp Blank

createQuestion :: GameStore -> IO ()
createQuestion gs = let currQ = currQuestion gs in 
  case findBlank currQ of
    1 -> createQuestionMain
    2 -> createQuestionInt
    3 -> createQuestionBool
    _ -> putStr "" -- this is still dumb
  where createQuestionMain = do putStrLn (questionOptions2 (currQuestion gs ))
                                input <- getLine
                                case readQuestionOptions2 input of
                                  Nothing -> do putStrLn "Invalid input, try again!"
                                                createQuestionMain
                                  Just q -> case buildQuestion (currQuestion gs) q of 
                                    Nothing -> undefined -- should be unreachable
                                    Just newQ -> createQuestion (gs {currQuestion = newQ})
        createQuestionInt = undefined
        createQuestionBool = undefined

readQuestionOptions2 :: String -> Maybe Question
readQuestionOptions2 s = 
  case readMaybe s :: Maybe Int of -- this should maybe be done with a map
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

move :: [Int] -> Game
move [] = return []
move (x:xs) = do
  return (xs ++ [x])
