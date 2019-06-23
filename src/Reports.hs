module Reports (Event(..), Aggregate(..), aggregate) where

import           Data.List
import           Data.Map

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
  show (MEq x)        = "=" ++ show x
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

newtype RDay = RDay String
data RHourOfDay = RHourOfDay String RDay
newtype RPaymentMethod = RPaymentMethod PaymentMethod
newtype RAmountBracket = RAmountBracket String
newtype RMerchantId = RMerchantId MerchantId

instance Show RDay where
  show (RDay x) = x
instance Show RHourOfDay where
  show (RHourOfDay h (RDay d)) = d ++ ":" ++ h
instance Show RPaymentMethod where
  show (RPaymentMethod x) = x
instance Show RAmountBracket where
  show (RAmountBracket x) = x
instance Show RMerchantId where
  show (RMerchantId x) = x

data Report = R0 RHourOfDay RAmountBracket |
              R1 RHourOfDay RAmountBracket RPaymentMethod |
              R2 RAmountBracket RPaymentMethod |
              R3 RDay RMerchantId |
              R4 RMerchantId RPaymentMethod

instance Show Report where
  show (R0 x y)   = show x ++ "|" ++ show y
  show (R1 x y z) = show x ++ "|" ++ show y ++ "|" ++ show z
  show (R2 x y)   = show x ++ "|" ++ show y
  show (R3 x y)   = show x ++ "|" ++ show y
  show (R4 x y)   = show x ++ "|" ++ show y

genReports :: Event -> [Report]
genReports e =
  [
    R0 hourOfDay amountBracket,
    R1 hourOfDay amountBracket paymentMethod,
    R2 amountBracket paymentMethod,
    R3 day merchantId,
    R4 merchantId paymentMethod
  ]
  where
    -- This code is unsafe, program will exit here on invalid input.
    --
    -- I wanted to fix it, but this excercise is just refactoring -
    -- it means that if old version of code exited here on
    -- invalid input then new version should exit as well.
    (d, 'T':h0:h1:_) = break (== 'T') $ eventIso8601Date e
    day :: RDay
    day = RDay d
    hourOfDay :: RHourOfDay
    hourOfDay = RHourOfDay [h0, h1] day
    paymentMethod :: RPaymentMethod
    paymentMethod = (RPaymentMethod . eventPaymentMethod) e
    amountBracket :: RAmountBracket
    amountBracket = (
        RAmountBracket .
        show .
        measure moneyScale moneyResolution .
        Money .
        eventMoneyAmount
      ) e
    merchantId :: RMerchantId
    merchantId = (RMerchantId . eventMerchantId) e

type AggSet = Map ReportData EventCount

aggregate :: [Event] -> [Aggregate]
aggregate es =
  foldlWithKey (\acc p c -> Agg p c : acc) [] aggSet
  where
    aggSet :: AggSet
    aggSet = Data.List.foldl go empty (show <$> (es >>= genReports))
    --
    --  Usage of ReportData type (which is String) as unique key
    --  to count events is not perfect idea because
    --  String representation of different reports
    --  theoretically can overlap, not now, but if we add more
    --  report types later - it can happen. Type system
    --  can't prevent this kind of errors. I did'n fixed it
    --  because excerice is just refactoring and behaviour
    --  of program should not change.
    --
    go :: AggSet -> ReportData -> AggSet
    go acc p =
      if member p acc
      then update (\x -> Just $ x + 1) p acc
      else Data.Map.insert p 1 acc

--
-- old version of code (before refactoring)
--
--
-- module Reports (Event(..), Aggregate(..), aggregate) where
--
-- type PaymentMethod = String
--
-- -- date amount method merchantID
-- data Event = Event String Int PaymentMethod String
--   deriving (Eq, Ord, Read)
--
-- -- datapoint events
-- data Aggregate = Agg String Int
--   deriving (Eq, Show)
--
-- hour :: Event -> String
-- hour (Event date _ _ _) = let (day, 'T':time) = break (== 'T') date
--                            in day ++ ":" ++ take 2 time
--
-- day :: Event -> String
-- day (Event date _ _ _) = takeWhile (/= 'T') $ date
--
-- amountBracket :: Event -> String
-- amountBracket (Event _ amount _ _) =
--   if (amount < 1000) then "<10"
--   else if (amount < 5000) then "10-50"
--   else if (amount < 10000) then "50-100"
--   else if (amount < 50000) then "100-500"
--   else ">500"
--
-- addAggregate :: String -> [Aggregate] -> [Aggregate]
-- addAggregate datapoint aggrs =
--   if any (\(Agg dp _) -> dp == datapoint) $ aggrs
--   then flip map [0..length aggrs - 1] $ \i ->
--     case aggrs !! i of
--       (Agg dp events) ->
--         if dp == datapoint
--         then Agg dp (events + 1)
--         else Agg dp events
--   else aggrs ++ [Agg datapoint 1]
--
-- aggregate :: [Event] -> [Aggregate]
-- aggregate events = foldl (\acc event ->
--   let (Event _ _ paymentMethod merchantID) = event in
--     addAggregate (hour event ++ "|" ++ amountBracket event) $
--     addAggregate (hour event ++ "|" ++ amountBracket event ++ "|" ++ paymentMethod) $
--     addAggregate (amountBracket event ++ "|" ++ paymentMethod) $
--     addAggregate (day event ++ "|" ++ merchantID) $
--     addAggregate (merchantID ++ "|" ++ paymentMethod) $
--     acc
--   ) [] events
