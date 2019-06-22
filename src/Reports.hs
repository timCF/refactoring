module Reports (Event(..), Aggregate(..), aggregate) where

import           Data.List

--
--  input types are just aliases
--  because it's refactoring
--  and we can't change public API
--

type Iso8601Date = String
type MoneyAmount = Int
type PaymentMethod = String
type MerchantId = String

data Event = Event {
  eventIso8601Date   :: Iso8601Date,
  eventMoneyAmount   :: MoneyAmount,
  eventPaymentMethod :: PaymentMethod,
  eventMerchantId    :: MerchantId
} deriving (Eq, Ord, Read)

--
--  output types are just aliases
--  because it's refactoring
--  and we can't change public API
--

type ReportData = String
type EventCount = Int

data Aggregate = Agg {
  aggReportData :: ReportData,
  aggEventCount :: EventCount
} deriving (Eq, Show)

--
-- new types and refactored code
--

newtype Money = Money MoneyAmount deriving (Eq, Ord)
instance Show Money where
  show m = show ma
    where Money ma = moneyResolution m

moneyResolution :: Money -> Money
moneyResolution (Money x) = Money $ div x 100

moneyScale :: [Money]
moneyScale = [
    Money 1000,
    Money 5000,
    Money 10000,
    Money 50000
  ]

data Measured a = MLt a | MEq a | MGt a | MBetween a a
instance (Show a) => Show (Measured a) where
  show (MLt x)        = "<" ++ show x
  show (MEq x)        = show x
  show (MGt x)        = ">" ++ show x
  show (MBetween x y) = show x ++ "-" ++ show y

measure :: (Ord a) => [a] -> (a -> a) -> a -> Measured a
measure scale resolution x = go (sort scale)
  where
    go [] = MEq x
    go [p]
      | x < p = MLt p
      | x > p = MGt p
      | otherwise = MEq p
    go (p0:p1:ps)
      | x < p0 = MLt p0
      | x >= p0 && x < p1 =
        case (resolution p0, resolution p1) of
          (p0', p1') | p0' == p1' -> go (p0:ps)
          (_, _)                  -> MBetween p0 p1
      | otherwise = go (p1:ps)

hour :: Event -> String
hour (Event date _ _ _) = let (d, 'T':time) = break (== 'T') date
                           in d ++ ":" ++ take 2 time

day :: Event -> String
day (Event date _ _ _) = takeWhile (/= 'T') date

amountBracket :: Event -> String
amountBracket =
  show . measure moneyScale moneyResolution . Money . eventMoneyAmount

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

-- reportHourAmount :: Event -> String
-- reportHourAmountPaymentMethod :: Event -> String
-- reportAmountPaymentMethod :: Event -> String
-- reportDayMerchant :: Event -> String
-- reportMerchantPaymentMethod :: Event -> String

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
