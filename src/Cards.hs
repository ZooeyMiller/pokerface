module Cards where

import Control.Monad
import Data.List
import Data.Maybe
import System.Random

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Eq, Show, Enum)

data Rank
  = Two
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
  | Ace
  deriving (Eq, Ord, Show, Enum)

data Card = Card
  { suit :: Suit,
    rank :: Rank
  }
  deriving (Show, Eq)

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

-- need a datatype for hand with the cards that make up that hand
data CardHand = CardHand Hand [Card] deriving (Show, Eq)

instance Ord CardHand where
  compare (CardHand hand1 _) (CardHand hand2 _) = compare hand1 hand2

-- It would be nice to use a vector or some kind of way to represent in the type
-- that we are only taking lists of 7, but that doesn't actually change the logic
getBestHand :: [Card] -> CardHand
getBestHand = maximum . getHands

-- here we need to get all possible hands for the given 7 cards, and then return the best hand from them
getHands :: [Card] -> [CardHand]
getHands hand =
  catMaybes $
    fmap
      ($ hand)
      [ getHighCard,
        getPairOrTwoPair,
        getThreeOfAKind,
        getStraight,
        getFlush,
        getFullHouse,
        getFourOfAKind,
        getStraightFlush,
        getRoyalFlush
      ]

safeTake :: Int -> [a] -> Maybe [a]
safeTake num list = reverse <$> go num list []
  where
    go 0 source res = Just res
    go toTake [] res = Nothing
    go toTake (forRes : source) res = go (toTake - 1) source (forRes : res)

orderCards :: [Card] -> [Card]
orderCards = sortOn rank

-- we need get functions for all of the potential hands
getHighCard :: [Card] -> Maybe CardHand
getHighCard cards = (CardHand HighCard) <$> resCards
  where
    resCards = safeTake 5 $ reverse $ orderCards cards

groupCards :: [Card] -> [[Card]]
groupCards cards = map getMatches cards
  where
    getMatches card = filter matchRank cards
      where
        matchRank cardFromList = rank cardFromList == rank card

makeLengthFive :: [a] -> [a] -> [a]
makeLengthFive toConcatTo toConcatFrom
  | length toConcatTo == 5 = toConcatTo
  | length toConcatTo > 5 = take 5 toConcatTo
  | otherwise = makeLengthFive (head toConcatFrom : toConcatTo) (tail toConcatFrom)

getPairOrTwoPair :: [Card] -> Maybe CardHand
getPairOrTwoPair cards = getRes pairs
  where
    pairs = nub $ filter ((== 2) . length) $ groupCards cards
    notPairs = nub $ filter ((/= 2) . length) $ groupCards cards
    resHand = makeLengthFive (join pairs) (join notPairs)
    getRes pairs'
      | length pairs' == 1 = Just $ CardHand Pair resHand
      | length pairs' > 1 = Just $ CardHand TwoPair resHand
      | otherwise = Nothing

getThreeOfAKind :: [Card] -> Maybe CardHand
getThreeOfAKind cards = getRes threes
  where
    threes = nub $ filter ((== 3) . length) $ groupCards cards
    notThrees = nub $ filter ((/= 3) . length) $ groupCards cards
    resHand = makeLengthFive (join threes) (join notThrees)
    getRes threes'
      | length threes' > 0 = Just $ CardHand ThreeOfAKind resHand
      | otherwise = Nothing

getStraight :: [Card] -> Maybe CardHand
getStraight = undefined

getFlush :: [Card] -> Maybe CardHand
getFlush = undefined

getFullHouse :: [Card] -> Maybe CardHand
getFullHouse = undefined

getFourOfAKind :: [Card] -> Maybe CardHand
getFourOfAKind = undefined

getStraightFlush :: [Card] -> Maybe CardHand
getStraightFlush = undefined

getRoyalFlush :: [Card] -> Maybe CardHand
getRoyalFlush = undefined

samplePairHand :: [Card]
samplePairHand = Card Spades Two : take 6 packOfCards

sampleTwoPairHand :: [Card]
sampleTwoPairHand = [Card Spades Two, Card Spades Three] ++ (take 5 packOfCards)

sampleThreeOfAKindHand :: [Card]
sampleThreeOfAKindHand = [Card Spades Two, Card Hearts Two] ++ (take 5 packOfCards)
