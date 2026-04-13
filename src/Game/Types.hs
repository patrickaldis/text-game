{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Game.Types where

import Data.Kind

type data Location = Start | S1

data Character = P1 | P2 | Narrator
  deriving (Show, Eq)

class Nameable a where
  name :: a -> String

instance Nameable Character where
  name P1 = "P1"
  name s = show s

data State (s :: Location) where
  StateStart :: State Start
  StateS1 :: State S1

data Frame = Frame
  { character :: Character
  , dialogue :: String
  }

newtype Sequence = Sequence [Frame]

data Action (pre :: Location) (post :: Location) where
  StartAction :: Action Start S1
  GoAway :: Action S1 S1
  SayHi :: Action S1 S1
deriving instance Show (Action pre post)

class HasActions (s :: Location) where
  allActions :: [Exists HasActions (Action s)]
instance HasActions Start where
  allActions = [Exists StartAction]
instance HasActions S1 where
  allActions = [Exists GoAway, Exists SayHi]

data SomeState = forall s. SomeState
  { nextState :: State s
  , sequence :: Sequence
  }

data Exists (c :: k -> Constraint) (t :: k -> Type) where
  Exists :: c a => t a -> Exists c t

withExists :: (forall a. c a => t a -> x) -> Exists c t -> x
withExists f (Exists x) = f x
