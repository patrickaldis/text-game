{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Types where

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

data SomeState = forall s. SomeState
  { nextState :: State s
  , sequence :: Sequence
  }
