{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module IOTest where
import GameState
import Game
import GamePieces
import Test.HUnit
import Test.QuickCheck
import qualified State as S
import qualified Data.DList as DL

-------------------- Fake IO --------------------

type FakeIO = S.State FakeState

data FakeState = FS
  { fsWrite :: DL.DList String    -- what has been written
  , fsInput :: [String]     -- what to read from
  }

instance Output FakeIO where
  write s = do
    st <- S.get
    let oldLog = fsWrite st
    let newLog = DL.append oldLog (DL.singleton s)
    S.put $ st { fsWrite = newLog }

instance Input FakeIO where
  input = do
    st <- S.get
    let (v,rest) = case fsInput st of
                     []     -> ("",[])
                     (x:xs) -> (x,xs)
    S.put $ st { fsInput = rest }
    return v

runFakeIO :: FakeIO () -> [String] -> [String]
runFakeIO comp inputs =
    DL.toList (fsWrite (S.execState comp initState))
  where
    initState = FS { fsWrite = DL.empty, fsInput = inputs }

fakeIOTest :: Test
fakeIOTest = 
  runFakeIO (play 3 0) ["quit"] ~?= [introText, commandsText, summaryText, promptText]
  -- TODO: build regression test