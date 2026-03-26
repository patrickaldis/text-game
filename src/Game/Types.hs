{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeData #-}

module Game.Types where
import Data.Kind (Type)

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

data StartAction = StartAction

data Frame = Frame Character String

newtype Sequence = Sequence [Frame]

type family Action (s :: Location) :: Type where
  Action Start = StartAction
  Action S1 = S1Action

-- Bar
-- Action:
--    Get Drink
--    Get Food
--    Exit

data S1Action = GoAway | SayHi

data SomeState = forall s. SomeState
  { nextState :: State s
  , sequence :: Sequence
  }
