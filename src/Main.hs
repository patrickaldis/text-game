{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Fix (MonadFix)
import Data.Char (digitToInt, isDigit)
import Data.Foldable (forM_)
import Data.Functor ((<&>))
import Data.List ((!?))
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
  advance <- input >>= updateState (initialStateFor StateStart StartAction)

  reg <- askRegion

  pane reg (pure True) $
    fill . constant $
      '.'

  _ <-
    runWithReplace
      ( let
          (_, Sequence (f : _)) = transition StateStart StartAction
         in
          col do
            tile flex blank
            tile (fixed 6) $
              drawFrame f
      )
      ( updated advance
          <&> ( \(GlobalState _ phase) ->
                  case phase of
                    Dialoguing (Sequence fs) i ->
                      col do
                        tile flex blank
                        tile (fixed 6) $
                          drawFrame (fs !! i)
                    Choosing f as ->
                      col do
                        tile flex $
                          row do
                            tile flex blank
                            tile flex $
                              col do
                                tile flex blank
                                tile (fixed (fromIntegral (length as + 2))) $
                                  boxTitle
                                    (constant doubleBoxStyle)
                                    "Actions"
                                    ( col
                                        ( forM_
                                            (zip [1 :: Integer ..] (withExists show <$> as))
                                            ( \(n, a) ->
                                                tile (fixed 1)
                                                  . text
                                                  . constant
                                                  . T.pack
                                                  $ show n ++ ". " ++ a
                                            )
                                        )
                                    )
                        tile (fixed 6) $
                          drawFrame f
              )
      )

  ctrlc

updateState ::
  (Reflex t, MonadHold t m, MonadFix m) =>
  GlobalState -> Event t V.Event -> m (Dynamic t GlobalState)
updateState = foldDyn \e gs -> case gs of
  GlobalState s phase -> case phase of
    Dialoguing sequence@(Sequence fs) i ->
      if i + 1 < length fs
        then GlobalState s (Dialoguing sequence (i + 1))
        else GlobalState s (Choosing (last fs) allActions)
    Choosing _ [] -> error "You Can't Advance"
    (Choosing _ as) ->
      let
        ma = do
          i <- getInt e
          as !? (i - 1)
       in
        case ma of
          Just (Exists a) -> initialStateFor s a
          Nothing -> GlobalState s phase

getInt :: V.Event -> Maybe Int
getInt (V.EvKey (V.KChar c) [])
  | isDigit c = Just . digitToInt $ c
  | otherwise = Nothing
getInt _ = Nothing

initialStateFor :: (HasActions s) => State s' -> Action s' s -> GlobalState
initialStateFor s a =
  let (s', Sequence fs) = transition s a
   in GlobalState s' (Dialoguing (Sequence fs) 0)

transition :: State s -> Action s s' -> (State s', Sequence)
transition = \case
  StateStart -> \case
    StartAction ->
      ( StateS1
      , Sequence
          [ Frame P1 "Hi here's text 1"
          , Frame P2 "Here's text 2"
          , Frame P1 "Here's text 3"
          ]
      )
  StateS1 -> \case
    SayHi -> (StateS1, Sequence [Frame P1 "Im In SayHi"])
    GoAway -> (StateS1, Sequence [Frame P1 "Im In GoAway"])

data GlobalState = forall s. GlobalState (State s) (Phase s)

data Phase (s :: Location) where
  Dialoguing :: (HasActions s) => Sequence -> Int -> Phase s
  Choosing :: Frame -> [Exists HasActions (Action s)] -> Phase s

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
