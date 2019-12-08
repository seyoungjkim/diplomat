# Diplomat Card Game

## Introduction

This is a command-line version of the card game, Diplomat! This game is similar to the popular game “Go Fish,” except more complicated questions are allowed (e.g. “Do you have the queen of spades?”, “Do you have at least three black cards?”, “Is the sum of the ranks of your non-face cards at least 50?”). Rather than hard-coding these yes-no questions, which are open-ended, we have created a DSL for questions which allows users to build up their own.

## Names
* Daria Fradkin, dfradkin
* Weizhen Sheng, wsheng
* Seyoung Kim, seyoungk

## File overview
### GamePieces.hs
The core data types, such as `Card`, `Rank`, `Suit`, and `Player`, are defined here. The files also includes functions to deal and shuffle the deck using a Random generator.

### Question.hs
This file contains our DSL for Questions. (TODO: expand)

### BuildQuestion.hs
Since we need to hold an "intermediate" Question state, which holds blank spots when a user hasn't finished creating their question, we define helper functions in this file to replace blanks with new inputs.

### Game.hs
This file, which contains the main functionality of the game, handles all IO functionality. (TODO: expand)

### Tests.hs
Contains unit tests, property tests, and a regression test.

### Main.hs, Setup.hs
These were auto-generated from `cabal init`.

### Dependencies
We use QuickCheck, HUnit, Random, and DList in addition to core Haskell libraries.

## How to build
Run `cabal build` followed by `cabal exec project`. Then you're playing!

## How to play

## TODO
* Finish README
* AI
* Fake IO Test
