module GameTexts where

import GamePieces (Rank, Suit, Card)
import Question (Question)
import Data.Set as Set (Set)

-------------------------------------------------------------------------------
-- Game Strings --

instructions :: String
instructions = "=== INSTRUCTIONS ===\n\
               \Diplomat is very similar to the popular card game, Go Fish.\n\
               \Cards are distributed evenly among all players, and then \n\
               \players take turns questioning each other. \n\nThe goal of \
               \the game is to collect as many ranks as possible,\nwhere \
               \claiming a rank means collecting all four cards of the\nrank \
               \(i.e. you can claim the Ace rank when you've located all\n\
               \four Aces in the game). \n\n\
               \On your turn, you can do the following actions:\n\n\
               \  1. Ask another player if they have a specific card (i.e.\n\
               \  Ace of Spades). If the anwer is yes, this card is laid out\n\
               \  and all players can see it, and you can continue your turn\n\
               \  and ask another question to any player. If the answer is\n\
               \  No, your turn ends.\n\n\
               \  2. Ask another player any Yes-No question about their hand\n\
               \  (i.e. 'Do you have any Black Queens?'). Regardless if the\n\
               \  answer is Yes or No, your turn ends.\n\n\
               \At any point during your turn, you can claim a rank if you\n\
               \know where all four cards of a rank are (AKA all four cards\n\
               \are either in your hand or laid out).\n\
               \====================\n"

commandsText :: String
commandsText = "Here are commands you can use:\n \
           \help: to view what commands you can use\n \
           \instr: to view Diplomat instructions\n \
           \hand: to view the cards in your hand\n \
           \laidout: to view currently laid-out cards\n \
           \claimed: to view the ranks you have claimed\n \
           \claim: to claim a rank\n \
           \ask: to ask another player a question\n \
           \quit: to quit the game.\n"

introText :: String
introText = "\nWelcome to Diplomat!\n"

promptText :: String
promptText = ">> Please type a command."

summaryText :: String
summaryText = "Summary since your last move:"

claimHelpText :: String
claimHelpText = "Here are commands you can use:\n \
                \help: to view what commands you can use\n \
                \nvm: to stop claiming a rank. \
                \quit: to quit the game.\n"

rankText :: String
rankText = "\n>> Enter a rank."

suitText :: String
suitText = "\n>> Enter a suit: Diamond, Club, Heart, or Spade."

emptySummaryText :: String
emptySummaryText = "\nThere is nothing to summarize :o\n"

emptyHandText :: String
emptyHandText = "\nYou have no cards in your hand left :o"

emptyLaidOutText :: String
emptyLaidOutText = "\nThere are no laid out cards :o"

emptyClaimedText :: String
emptyClaimedText = "\nYou haven't claimed any ranks yet :o"

filterOutInText :: String
filterOutInText = 
  "\n>> Would you like to filter cards out or filter cards in?\n\
  \ 1: Filter out \n\
  \ 2: Filter in"

filterOutText :: String
filterOutText = 
  "\n>> What would you like to filter out? Enter any rank, suit, or" ++ 
  " 'done' if there's nothing else to filter."

filterInText :: String
filterInText = 
  "\n>> What would you like to filter in? Enter any rank, suit, or" ++ 
  " 'done' if there's nothing else to filter."

errorText :: String
errorText = "System error!"

invalidInputText :: String
invalidInputText = "\nInvalid input, try again!"

lineBreakText :: String
lineBreakText = "\n===========\n"

-------------------------------------------------------------------------------
-- Helper functions that return a Game String --

-- | Helper function for representing an answer response
showBool :: Bool -> String
showBool True = "Yes! :)"
showBool False = "No :("

askText :: Int -> Question -> String
askText pid q = "\nYou asked Player " ++ show pid ++ ": " ++ "\n" ++ show q

askSuccessText :: Card -> String
askSuccessText c = "Yes, they had the " ++ show c ++ lineBreakText

askFailureText :: Card -> String
askFailureText c = "No, they didn't have the " ++ show c ++ "\n" ++ lineBreakText

newTurnText :: Int -> String
newTurnText i = ">> It's Player " ++ show i ++ 
  "'s turn!! Press <enter> to continue."

oldTurnText :: Int -> String
oldTurnText i = "It's still Player " ++ show i ++ "'s turn :)\n"

filteredInSoFarText :: Set Suit -> Set Rank -> String
filteredInSoFarText filteredSuits filteredRanks = 
  "\nSo far, you've filtered in:\n"
  ++ show filteredSuits ++ "\n" ++ show filteredRanks

filteredOutSoFarText :: Set Suit -> Set Rank -> String
filteredOutSoFarText filteredSuits filteredRanks = 
  "\nSo far, you've filtered out:\n"
  ++ show filteredSuits ++ "\n" ++ show filteredRanks

claimRankSuccessText :: Rank -> String
claimRankSuccessText r = "\nCongrats, you successfully claimed the rank "
  ++ show r ++ " :)\n"

enterIdText :: Int -> String
enterIdText i = "\n>> Player " ++ show i ++ 
  ": Enter a player id to choose a player to ask a question to."

enterQuestionText :: Int -> String
enterQuestionText i = "\n>> Player " ++ show i ++ ": Enter a question."

endText :: Int -> String
endText i = lineBreakText ++ "🎉🎉 Player " ++ show i ++ " won! 🎉🎉\n\n" ++
  "Here are how many ranks everyone collected:\n"
  