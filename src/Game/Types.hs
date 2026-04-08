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

data Frame = Frame Character String

newtype Sequence = Sequence [Frame]

data Action (pre :: Location) (post :: Location) where
  StartAction :: Action Start S1
  GoAway :: Action S1 S1
  SayHi :: Action S1 S1

-- data SomeAction pre where
--   SomeAction :: Action pre post -> SomeAction pre

-- Bar
-- Action:
--    Get Drink
--    Get Food
--    Exit


data SomeState = forall s. SomeState
  { nextState :: State s
  , sequence :: Sequence
  }
