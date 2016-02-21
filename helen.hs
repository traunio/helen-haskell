

module Helen_skaba where
-- Copyright (c) 2016: Tapani Raunio, tapani.raunio@gmail.com

-- Price peaks and lows, i.e. local maxima and minima.
-- Constructor tells the whether first value is maximum or minimum

type Price = Float

type Year =   Int
type Month =  Int
type Day =    Int
type Hours =  String -- e.g "00-01"
data Date = D Day Month Year

data TimeStamp = TS Date Hours

data PriceData a b = PD { tag:: a, value::b}
  deriving (Show)

firstEx :: (b -> b -> Bool) -> [PriceData a b] -> [PriceData a b]
firstEx _ [] = []
firstEx _ [_] = []
firstEx f (x:y:xs)
  | value x `f` value y     = [x]
  | value y `f` value x     = []
  | otherwise = firstEx f (x:xs)

lastEx :: (b -> b -> Bool) -> [PriceData a b] -> [PriceData a b]
lastEx f = firstEx f . reverse

localEx :: (Eq b) =>  (b -> b -> Bool) -> [PriceData a b] -> [PriceData a b]
localEx _ [] = []
localEx _ [_] = []
localEx f [x,y]
  | value x `f` value y = [x]
  | value y `f` value x = [y]
  | otherwise = []
localEx f xs = firstEx f xs ++ lEx f xs ++ lastEx f xs

lEx :: (Eq b) => (b -> b -> Bool) -> [PriceData a b] -> [PriceData a b]
lEx _ [] = []
lEx _ [_] = []
lEx _ [_,_] = []
lEx f (x:y:z:xs)
    | value x == value y                         = lEx f (x:z:xs)
    | value y `f` value x && value y `f` value z = y : lEx f (y:z:xs)
    | value y == value z                         = lEx f (x:z:xs)
    | otherwise                                  = lEx f (y:z:xs)

localMax :: (Ord b) => [PriceData a b] -> [PriceData a b]
localMax = localEx (>)

localMin :: (Ord b) => [PriceData a b] -> [PriceData a b]
localMin = localEx (<)


-- localMax :: [Price] -> [Price]
-- localMax = localEx (>)
--


-- first try:
-- firstEx' :: (Price -> Price -> Bool) -> [Price] -> [Price]
-- firstEx' _ [] = []
-- firstEx' _ [_] = []
-- firstEx' f (x:y:xs)
--   | x `f` y     = [x]
--   | y `f` x     = []
--   | otherwise = firstEx' f (x:xs)
--
-- lastEx :: (Price -> Price -> Bool) -> [Price] -> [Price]
-- lastEx f = firstEx f . reverse
--
-- localEx :: (Price -> Price -> Bool) -> [Price] -> [Price]
-- localEx _ [] = []
-- localEx _ [_] = []
-- localEx f [x,y]
--   | x `f` y = [x]
--   | y `f` x = [y]
--   | otherwise = []
-- localEx f xs = firstEx f xs ++ lEx f xs ++ lastEx f xs
--
-- lEx :: (Price -> Price -> Bool) -> [Price] -> [Price]
-- lEx _ [] = []
-- lEx _ [_] = []
-- lEx _ [_,_] = []
-- lEx f (x:y:z:xs)
--     | x == y = lEx f (x:z:xs)
--     | y `f` x && y `f` z = y : lEx f (y:z:xs)
--     | y == z = lEx f (x:z:xs)
--     | otherwise = lEx f (y:z:xs)
--
-- localMax :: [Price] -> [Price]
-- localMax = localEx (>)
--
-- localMin :: [Price] -> [Price]
-- localMin = localEx (<)
