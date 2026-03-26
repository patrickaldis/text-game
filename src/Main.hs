{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Control.Monad.Fix (MonadFix)
import Data.Text qualified as T
import Game.Types hiding (text)
import Graphics.Vty qualified as V
import Reflex
import Reflex.Network (networkView)
import Reflex.Vty
import Prelude hiding (sequence)

main :: IO ()
main = mainWidget $ do
  initManager_ do
    app

app ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , HasInput t m
  , HasTheme t m
  , HasDisplayRegion t m
  , HasImageWriter t m
  , HasFocusReader t m
  , HasFocus t m
  , HasLayout t m
  , Adjustable t m
  , PostBuild t m
  , NotReady t m
  ) =>
  m (Event t ())
app = do
  -- Read input at the app level
  ev <- input
  let qPressed = fforMaybe ev $ \case
        V.EvKey (V.KChar 'q') [] -> Just ()
        _ -> Nothing

  col do
    tile flex $
      text "picture here"

    tile (fixed 6) $
      drawSequence qPressed seq0

transition :: State s -> Action s -> (State s', Sequence)
transition = \case
  StateStart -> \case
    StartAction ->
      (StateS1, 
        Sequence
          [ Frame P1 "Hi here's text 1"
          , Frame P2 "Now I'm saying something"
          , Frame P1 "I stink"
          ]
        )
  StateS1 -> \case
    SayHi -> (StateStart, Sequence [])
    _ -> (StateStart, Sequence [])



data ChatState s = ChatState
  { s0 :: State s
  , a :: Action s
  , i :: Int
  }

nextState :: ChatState s -> Either (ChatState s) (State s)
nextState state | i state == len seqLen = Right $ transition (s0 state) (a state)

drawFrame ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , HasTheme t m
  , HasDisplayRegion t m
  , HasImageWriter t m
  , HasFocusReader t m
  , HasInput t m
  ) =>
  Frame ->
  m ()
drawFrame (Frame c t) =
  boxTitle
    (constant thickBoxStyle)
    (constant . T.pack $ name c)
    (text . constant . T.pack $ t)
