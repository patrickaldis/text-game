{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.Fix (MonadFix)
import Data.Functor (void)
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
  advance <- keyCombo (V.KEnter, []) >>= f (ChatState StateStart StartAction 0 Nothing) . void

  col do
    tile flex $
      text "picture here"

    tile (fixed 6) $
      drawCurrentFrame advance

    ctrlc

f :: (Reflex t, MonadHold t m, MonadFix m) => ChatState s s' -> Event t () -> m (Dynamic t (ChatState s s'))
f = foldDyn \_ s ->
  case nextFrame s of
    Left s' -> s'
    Right _ -> error "unimplemented"

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

data ChatState s s' = ChatState
  { initialState :: State s
  , action :: Action s s'
  , frameNo :: Int
  , atChoice :: Maybe [State s']
  }

getFrame :: ChatState s s' -> Frame
getFrame state =
  case snd . transition (initialState state) $ action state of
    Sequence fs -> fs !! frameNo state

nextFrame :: ChatState s s' -> Either (ChatState s s') (State s')
nextFrame state =
  if frameNo state + 1 < length y
    then Left state{frameNo = i'}
    else Right x
 where
  (x, Sequence y) = transition (initialState state) (action state)
  i' = frameNo state + 1

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
  Dynamic t Frame ->
  m ()
drawFrame df = do
  boxTitle
    (constant thickBoxStyle)
    (current . fmap (T.pack . name . character) $ df)
    (text . current . fmap (T.pack . dialogue) $ df)

drawCurrentFrame ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , HasTheme t m
  , HasDisplayRegion t m
  , HasImageWriter t m
  , HasFocusReader t m
  , HasInput t m
  ) =>
  Dynamic t (ChatState s s') ->
  m ()
drawCurrentFrame state = drawFrame (getFrame <$> state)
