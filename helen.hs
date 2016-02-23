
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
