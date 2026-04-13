{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Control.Monad.Fix (MonadFix)
import Data.Functor (void, (<&>))
import Data.Text qualified as T
import Game.Types hiding (nextState)
import Graphics.Vty.Input qualified as V
import Reflex
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
  , MonadHold t m
  ) =>
  m (Event t ())
app = do
  advance <- keyCombo (V.KEnter, []) >>= f (initialStateFor StateStart StartAction) . void

  _ <-
    runWithReplace
      (col tile flex $ text "Init")
      ( updated advance
          <&> ( \(GlobalState s phase) -> col do
                  tile flex $
                    text "picture here"
                  case phase of
                    Dialoguing a (Sequence fs) i -> 
                      tile (fixed 6) $
                        drawFrame (fs !! i) 
              )
      )

  ctrlc

f ::
  (Reflex t, MonadHold t m, MonadFix m) =>
  GlobalState -> Event t () -> m (Dynamic t GlobalState)
f = foldDyn \_ gs -> case gs of
  GlobalState s phase -> case phase of
    Dialoguing a sequence@(Sequence fs) i ->
      if i + 1 < length fs
        then GlobalState s (Dialoguing a sequence (i + 1))
        else GlobalState s (Choosing allActions)
    Choosing ((Exists a) : as) ->
      initialStateFor s a

initialStateFor :: HasActions s => State s' -> Action s' s -> GlobalState
initialStateFor s a =
  let (s', Sequence fs) = transition s a
   in GlobalState s' (Dialoguing a (Sequence fs) 0)

transition :: State s -> Action s s' -> (State s', Sequence)
transition = \case
  StateStart -> \case
    StartAction ->
      ( StateS1
      , Sequence
          [ Frame P1 "Hi here's text 1"
          , Frame P2 "Now I'm saying something"
          , Frame P1 "I stink"
          ]
      )
  StateS1 -> \case
    SayHi -> (StateS1, Sequence [])
    GoAway -> (StateS1, Sequence [])

data GlobalState = forall s. GlobalState (State s) (Phase s)

data Phase (s :: Location) where
  Dialoguing :: HasActions s => Action s' s -> Sequence -> Int -> Phase s
  Choosing :: [Exists HasActions (Action s)] -> Phase s

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
drawFrame f = do
  boxTitle
    (constant thickBoxStyle)
    (constant . T.pack . name . character $ f)
    (text . constant . T.pack . dialogue $ f)
