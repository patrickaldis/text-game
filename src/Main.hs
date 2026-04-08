{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Control.Monad.Fix (MonadFix)
import Data.Text qualified as T
import Game.Types hiding (nextState, text)
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

  col do
    tile flex $
      text "picture here"

    tile (fixed 6) $
      drawSequence ChatState {s0=StateStart, a=StartAction, i=0}

-- drawSequence :: (Monad m, HasInput t m, Reflex t) => ChatState s s' -> m (Event t ())
drawSequence ::
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
  ChatState Start S1 -> m (Event t ())
drawSequence s = do
  case transition (s0 s) (a s) of
    (StateS1, Sequence seq') -> do
      -- Read input at the app level
      ev <- input
      let qPressed = fforMaybe ev $ \case
            V.EvKey (V.KChar 'q') [] -> Just ()
            _ -> Nothing
      drawFrame $ seq' !! i s
      case nextState s of
        Left e -> _
        Right r -> _
      pure qPressed

transition :: State s -> Action s s' -> (State s', Sequence)
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
    SayHi -> (StateS1, Sequence [])
    GoAway -> (StateS1, Sequence [])

data ChatState s s' = ChatState
  { s0 :: State s
  , a :: Action s s'
  , i :: Int
  }

nextState :: ChatState s s' -> Either (ChatState s s') (State s')
nextState state | i state == length y = Right x
  where
    (x, Sequence y) = transition (s0 state) (a state)
nextState state = Left state{i = i state + 1}

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
