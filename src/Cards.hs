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
  initialLength <- pure $ length cards - 1

  -- get me a random index in the list
  index <- getStdRandom $ randomR (0, initialLength)

  -- get the card at that index, and the list with that card removed
  let (card, newCards) = removeCard index cards

  -- if the card existed, recurse while concatenating, else return just the newCards (an empty list at this point)
  case card of
    Just c -> fmap (c :) (shuffle newCards)
    Nothing -> pure newCards

removeCard :: Int -> [Card] -> (Maybe Card, [Card])
removeCard i cs = go 0 i cs []
  where
    go curr i' (c : cs') rcs = if curr == i' then (Just c, cs' ++ rcs) else go (curr + 1) i' cs' (c : rcs)
    go curr i' [] rcs = (Nothing, rcs)
