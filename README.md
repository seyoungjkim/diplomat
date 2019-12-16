# Diplomat Card Game

## Introduction

This is a command-line version of the card game, Diplomat! This game is similar to the popular game “Go Fish,” except more complicated questions are allowed (e.g. “Do you have the queen of spades?”, “Do you have at least three black cards?”, “Is the sum of the ranks of your non-face cards at least 50?”). Rather than hard-coding these yes-no questions, which are open-ended, we have created a DSL for questions which allows users to build up their own.

## Names
* Daria Fradkin, dfradkin
* Weizhen Sheng, wsheng
* Seyoung Kim, seyoungk

## Instructions
Diplomat is very similar to the popular card game, Go Fish. Cards are distributed evenly among all players, and then players take turns questioning each other. 
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
This file contains our DSL for Questions. (TODO: expand)

### BuildQuestion.hs
Since we need to hold an "intermediate" Question state, which holds blank spots when a user hasn't finished creating their question, we define helper functions in this file to replace blanks with new inputs.

### Game.hs
This file, which contains the main functionality of the game, handles all IO functionality. (TODO: expand)

### AI.hs
(TODO: exapnd)

### Tests.hs
Contains unit tests, QuickCheck property tests, and a regression test.

### Main.hs, Setup.hs
These were auto-generated from `cabal init`.

### Dependencies
We use QuickCheck, HUnit, Random, and DList in addition to core Haskell libraries.

## How to build
Run `cabal new-build` followed by `cabal new-exec diplomat NUM_PLAYERS NUM_AI`. Then you're playing!
