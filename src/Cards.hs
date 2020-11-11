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

data Hand
  = HighCard
  | Pair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  | RoyalFlush
  deriving (Ord, Eq, Show)

data Error = PackEmptyError deriving (Show)

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

deal :: Int -> [Card] -> Either Error ([Card], [Card])
deal amountToDeal cards = go amountToDeal [] cards
  where
    go 0 dealt pack = Right (dealt, pack)
    go _ _ [] = Left PackEmptyError
    go amt dealt (head : pack) = go (amt - 1) (head : dealt) pack

-- It would be nice to use a vector or some kind of way to represent in the type
-- that we are only taking lists of 7, but that doesn't actually change the logic
getHand :: [Card] -> Hand

-- here we need to get all possible hands for the given 7 cards, and then return the best hand from them
