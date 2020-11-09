module Cards where

import System.Random

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

-- can I specify unique list?
packOfCards :: [Card]
packOfCards =
  do
    suit <- [toEnum 0 ..]
    rank <- [toEnum 0 ..]
    pure $ Card suit rank

shuffle :: [Card] -> IO [Card]
shuffle cards = do
  -- get me a random index in the list
  index <- getStdRandom $ randomR (0, length cards - 1)
  -- get the card at that index, and the list with that card removed
  let (card, newCards) = removeCard index cards

  case card of
    Just c -> fmap (c :) (shuffle newCards)
    Nothing -> pure newCards

removeCard :: Int -> [Card] -> (Maybe Card, [Card])
removeCard goalIndex cards = go 0 goalIndex cards []
  where
    go currentIndex goalIndex' (card : cards') cardsToReturn =
      if currentIndex == goalIndex'
        then (Just card, cards' ++ cardsToReturn)
        else go (currentIndex + 1) goalIndex' cards' (card : cardsToReturn)
    go _ _ [] cardsToReturn = (Nothing, cardsToReturn)
