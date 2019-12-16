# ♦️♣️ Diplomat Card Game ♥️♠️

## Names
* Daria Fradkin (dfradkin)
* Weizhen Sheng (wsheng)
* Seyoung Kim (seyoungk)

## Introduction

This is a command-line version of the card game, Diplomat! This game is similar to the popular game “Go Fish,” except *any* Yes-No questions about a player's hand are allowed (e.g. “Do you have the queen of spades?”, “Do you have at least three black cards?”, “Is the sum of the ranks of your non-face cards at least 50?”). Rather than hard-coding these Yes-No questions, which are open-ended, we have created a DSL which allows users to build up their own questions.

## Instructions
Diplomat is very similar to the popular card game, "Go Fish." Cards are distributed evenly among all players, and then players take turns questioning each other. 
The goal of the game is to collect as many ranks as possible, where claiming a rank means collecting all four cards of the rank (i.e. you can claim the Ace rank when you've located all four Aces in the game).
On your turn, you can do the following actions:
1. Ask another player if they have a specific card (i.e. Ace of Spades). If the anwer is yes, this card is laid out and all players can see it, and you can continue your turn and ask another question to any player. If the answer is No, your turn ends.
2. Ask another player any Yes-No question about their hand (i.e. 'Do you have any Black Queens?'). Regardless if the answer is Yes or No, your turn ends.
    At any point during your turn, you can claim a rank if you know where all four cards of a rank are (AKA all four cards are either in your hand or laid out).
 
Here are commands you can use:
* help: to view what commands you can use
* instr: to view Diplomat instructions
* hand: to view the cards in your hand
* laidout: to view currently laid-out cards
* claimed: to view the ranks you have claimed
* claim: to claim a rank
* ask: to ask another player a question
           quit: to quit the game.

## File overview
### GamePieces.hs
The core data types, such as `Card`, `Rank`, `Suit`, and `Player`, are defined here. The files also includes functions to deal and shuffle the deck using a Random generator.

### Question.hs
This file contains our DSL for Questions. The DSL is comprised of three main components, grouped by
what the component results in:
* `Question` (a main Question)
* `QInt` (a question that results in an Int value)
* `QHand` (a question that results in a Hand)

### BuildQuestion.hs
We need an "intermediate" Question state for users to build up Questions with, which holds blank spots when a user hasn't finished creating their question. We define helper functions in this file to replace blanks with new inputs.

### GameState.hs
This is the core game logic which does not depend on any input. It contains `Game`, which uses the `State` monad and a custom-designed `GameStore` to maintain game data. It handles initialization, laying out cards, and claiming ranks.

### GameTexts.hs
This file contains all Strings that are displayed in the Game UI (i.e. instructions), as well as very simple functions which return Strings to display (i.e. `endText`, which returns a String specifying which player won).

### Game.hs
This file, which contains the main functionality of the game, handles all input and output. This includes allowing a user to interactively create a complex question using our DSL.

### AI.hs
This file contains the AI logic. It updates an AI's guesses about the state of the game (i.e. which player could potentially have which cards), tries to claim ranks, and asks questions about specific cards to players it thinks may have the card.

### Tests.hs, IOTest.hs
Contain unit tests, QuickCheck property tests, and a comprehensive fake IO test. All tests can be run using `main` in `Tests.hs`.

### Main.hs, Setup.hs
These were auto-generated from `cabal init`.

### Dependencies and Acknowledgments
We use QuickCheck, HUnit, Random, and DList in addition to core Haskell libraries. 
We used [Random shuffle functionality from the Haskell wiki](https://wiki.haskell.org/Random_shuffle). We also use the `Input` and `Output` monads and `FakeIO` types from the [Concurrency Monad Transformer in-class exercise](https://www.cis.upenn.edu/~cis552/current/lectures/soln/TransC.html) for testing purposes.

## How to build
Run `cabal new-build` to compile and `cabal new-exec diplomat [NUM_PLAYERS] [NUM_AI]` to play the game, where `NUM_PLAYERS` denotes the number of human players and `NUM_AI` denotes the number of computer players. 
