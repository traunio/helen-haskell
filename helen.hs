
module Helen_skaba where
-- Copyright (c) 2016: Tapani Raunio, tapani.raunio@gmail.com

-- Price peaks and lows, i.e. local maxima and minima.
-- Constructor tells the whether first value is maximum or minimum


type Price = Float

type Year =   Int
type Month =  Int
type Day =    Int
type Hours =  String -- e.g "00-01"
data Date = D Day Month Year deriving Show

data TimeStamp = TS Date Hours deriving Show

-- Both a and b should be Ord instances
data PriceData a b = PD { tag::a, value::b} deriving (Show)

type ChargeLoss = Float -- value between 0-1. Amount of charge lost

-- Requirements for buying and selling
data Reqs = Full ChargeLoss Price | Empty ChargeLoss deriving (Show)

instance Eq TimeStamp where
  (==) (TS (D d1 m1 y1) s1 ) (TS (D d2 m2 y2) s2) = yy && dd && mm && ss
      where yy = y1 == y2
            mm = m1 == m2
            dd = d1 == d2
            ss = s1 == s2

instance Ord TimeStamp where
  compare (TS (D d1 m1 y1) s1 ) (TS (D d2 m2 y2) s2)
      | y1 < y2   = LT
      | y2 > y1   = GT
      | m1 < m2   = LT
      | m2 > m1   = GT
      | d1 < d2   = LT
      | d2 > d2   = GT
      | s1 < s2   = LT
      | s2 > s1   = GT
      | otherwise = EQ

-- Checks whether first item is max/min.
-- Consecutive values are discarded
firstEx :: (b -> b -> Bool) -> [PriceData a b] -> [PriceData a b]
firstEx _ [] = []
firstEx _ [_] = []
firstEx f (x:y:xs)
  | value x `f` value y     = [x]
  | value y `f` value x     = []
  | otherwise = firstEx f (x:xs)

lastEx :: (b -> b -> Bool) -> [PriceData a b] -> [PriceData a b]
lastEx f = firstEx f . reverse

-- Helper function for calculating local min or max
localEx :: (Eq b) =>  (b -> b -> Bool) -> [PriceData a b] -> [PriceData a b]
localEx _ [] = []
localEx _ [_] = []
localEx f [x,y]
  | value x `f` value y = [x]
  | value y `f` value x = [y]
  | otherwise = []
localEx f xs = firstEx f xs ++ lEx f xs ++ lastEx f xs

-- Helper function for localEx
lEx :: (Eq b) => (b -> b -> Bool) -> [PriceData a b] -> [PriceData a b]
lEx _ [] = []
lEx _ [_] = []
lEx _ [_,_] = []
lEx f (x:y:z:xs)
    | value x == value y                         = lEx f (x:z:xs)
    | value y `f` value x && value y `f` value z = y : lEx f (y:z:xs)
    | value y == value z                         = lEx f (x:z:xs)
    | otherwise                                  = lEx f (y:z:xs)

-- Calculates the local maxima (including end points)
localMax :: (Ord b) => [PriceData a b] -> [PriceData a b]
localMax = localEx (>)

-- Calculate the local minima (including end points)
localMin :: (Ord b) => [PriceData a b] -> [PriceData a b]
localMin = localEx (<)

-- Calculates the price of buy/sell pattern. Version 2 with fold
-- Version 2, using fold
calcPrice :: Reqs -> [PriceData a Price] -> Price
calcPrice _ [] = 0
calcPrice (Full loss price) [x] = (1-loss)*value x - price
calcPrice a@(Full loss _) (x:xs) =
      calcPrice a [x] + calcPrice (Empty loss) xs
calcPrice (Empty loss) xs = revenue*(1-loss) - costs
            where tupper = foldl f (False, 0, 0) xs
                  revenue = let (_,sells,_)=tupper in sells
                  costs = let (_,_,buys)=tupper in buys
                  f acc x = let (full, sells, buys) = acc
                            in if full then (False, sells + value x, buys)
                                  else (True, sells, buys+ value x)


-- Function input: Reqs, maxs, mins
-- Function output: most money :)
-- bestMoney :: Reqs -> [PriceData a b] -> [PriceData a b] -> [PriceData a b]
-- bestMoney _ _ _ = undefined
-- bestMoney _ [] _ = []
-- bestMoney _ _ [] = []
-- bestMoney (Full diff) highs lows =


-- step 1. calculate possible prices


-- bestMoney (Empty diff) b c = undefined
