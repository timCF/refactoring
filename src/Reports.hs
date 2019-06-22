module Reports (Event(..), Aggregate(..), aggregate) where

import           Data.List

type Iso8601Date = String
type CentAmount = Int
type PaymentMethod = String
type MerchantId = String

data Event = Event {
  eventIso8601Date   :: Iso8601Date,
  eventCentAmount    :: CentAmount,
  eventPaymentMethod :: PaymentMethod,
  eventMerchantId    :: MerchantId
} deriving (Eq, Ord, Read)

type ReportData = String
type EventCount = Int

data Aggregate = Agg {
  aggReportData :: ReportData,
  aggEventCount :: EventCount
} deriving (Eq, Show)

newtype MoneyAmount = MoneyAmount CentAmount deriving (Eq, Ord)
instance Show MoneyAmount where
  show (MoneyAmount ma) = show $ div ma 100

moneyAmountScale :: [MoneyAmount]
moneyAmountScale = [
    MoneyAmount 1000,
    MoneyAmount 5000,
    MoneyAmount 10000,
    MoneyAmount 50000
  ]

showOnScale :: (Ord a, Show a) => [a] -> a -> String
showOnScale xs x = go (sort xs)
  where
    go [] = show x
    go [p]
      | x < p = "<" ++ show p
      | x > p = ">" ++ show p
      | otherwise = show p
    go (p0:p1:ps)
      | x < p0 = "<" ++ show p0
      | x >= p0 && x < p1 =
        case (show p0, show p1) of
          (s0, s1) | s0 == s1 -> go (p0:ps)
          (s0, s1)            -> s0 ++ "-" ++ s1
      | otherwise = go (p1:ps)

hour :: Event -> String
hour (Event date _ _ _) = let (d, 'T':time) = break (== 'T') date
                           in d ++ ":" ++ take 2 time

day :: Event -> String
day (Event date _ _ _) = takeWhile (/= 'T') date

amountBracket :: Event -> String
amountBracket =
  showOnScale moneyAmountScale . MoneyAmount . eventCentAmount

addAggregate :: String -> [Aggregate] -> [Aggregate]
addAggregate datapoint aggrs =
  if any (\(Agg dp _) -> dp == datapoint) aggrs
  then flip map [0..length aggrs - 1] $ \i ->
    case aggrs !! i of
      (Agg dp events) ->
        if dp == datapoint
        then Agg dp (events + 1)
        else Agg dp events
  else aggrs ++ [Agg datapoint 1]

aggregate :: [Event] -> [Aggregate]
aggregate = foldl (\acc event ->
  let (Event _ _ paymentMethod merchantID) = event in
    addAggregate (hour event ++ "|" ++ amountBracket event) $
    addAggregate (hour event ++ "|" ++ amountBracket event ++ "|" ++ paymentMethod) $
    addAggregate (amountBracket event ++ "|" ++ paymentMethod) $
    addAggregate (day event ++ "|" ++ merchantID) $
    addAggregate (merchantID ++ "|" ++ paymentMethod)
    acc
  ) []
