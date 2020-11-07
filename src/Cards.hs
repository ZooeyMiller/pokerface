module Cards where

import Data.Set

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Eq, Show, Enum)

data Rank
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving (Eq, Ord, Show, Enum)

data Card = Card
  { suit :: Suit,
    rank :: Rank
  }
  deriving (Show)

packOfCards :: Set Card
packOfCards =
  fromDistinctAscList $ do
    suit <- [toEnum 0 ..]
    rank <- [toEnum 0 ..]
    pure $ Card suit rank
