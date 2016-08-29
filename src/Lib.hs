module Lib (mainFunc) where
 
import System.IO
import Data.List
import System.Console.GetOpt
import Control.Monad (when)
import System.Environment

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
      | y1 > y2   = GT
      | m1 < m2   = LT
      | m1 > m2   = GT
      | d1 < d2   = LT
      | d1 > d2   = GT
      | s1 < s2   = LT
      | s1 > s2   = GT
      | otherwise = EQ

-- Checks whether first item is max/min.
-- Consecutive values are discarded
firstEx :: (b -> b -> Bool) -> [PriceData a b] -> [PriceData a b]
firstEx _ [] = []
firstEx _ [_] = []
firstEx f (x:y:xs)
  | value x `f` value y     = [x]
  | value y `f` value x     = []
  | otherwise = firstEx f (x:xs)

lastEx :: (b -> b -> Bool) -> [PriceData a b] -> [PriceData a b]
lastEx f = firstEx f . reverse

-- Helper function for calculating local min or max
localEx :: (Eq b) =>  (b -> b -> Bool) -> [PriceData a b] -> [PriceData a b]
localEx _ [] = []
localEx _ [_] = []
localEx f [x,y]
  | value x `f` value y = [x]
  | value y `f` value x = [y]
  | otherwise = []
localEx f xs = firstEx f xs ++ lEx f xs ++ lastEx f xs

-- Helper function for localEx
lEx :: (Eq b) => (b -> b -> Bool) -> [PriceData a b] -> [PriceData a b]
lEx _ [] = []
lEx _ [_] = []
lEx _ [_,_] = []
lEx f (x:y:z:xs)
    | value x == value y                         = lEx f (x:z:xs)
    | value y `f` value x && value y `f` value z = y : lEx f (y:z:xs)
    | value y == value z                         = lEx f (x:z:xs)
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

-- Helper function for bestMoney
validTime :: (Ord a) => PriceData a Price -> [PriceData a Price]
  -> [PriceData a Price]
validTime y = filter (\x -> tag y < tag x)

-- Calculates the optimal solutions
-- Input: battery state (loss, pric), highs, lows,
-- Output: (price, optimal solution)
bestMoney :: (Ord a) => Reqs ->   [PriceData a Price] -> [PriceData a Price]
  -> (Price, [PriceData a Price])
bestMoney (Full _ _) [] _ = (0, [])
bestMoney (Empty _) _ []  = (0, [])
bestMoney (Full loss price) highs lows =
  foldr (\x acc -> if fst acc < fst x then x else acc) (0,[]) combs
  where validHighs = filter (\x -> (1- loss)  * value x - price > 0)
        lambda x = let val = validTime x
                       rest = bestMoney (Empty loss) (val highs) (val lows)
                       a = fst rest + (1-loss) * value x - price
                       b = x: snd rest
                    in (a,b)
        combs = map lambda (validHighs highs)
bestMoney (Empty loss) highs lows =
  foldr (\x acc -> if fst acc < fst x then x else acc) (0,[]) combs
  where validHighs y = filter (\x -> (1-loss) * value x - value y > 0)
        valids y = validTime y . validHighs y
        lambda x acc= if null (valids x highs) then acc else x:acc
        validLows = foldr lambda [] lows
        combine x = let valHighs = validTime x highs
                        valLows = validTime x lows
                        rest = bestMoney (Full loss (value x)) valHighs valLows
                    in (fst rest, x:snd rest)
        combs = map combine validLows



-- Options
data Options = Options
 { optInteractive :: Bool
 , optLoss        :: ChargeLoss
 , optOutput      :: String
 } deriving Show

defaultOptions    = Options
 { optInteractive = False
 , optLoss        = 0.2 
 , optOutput      = "results.txt"
 }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['i'] ["interactive"]
     (NoArg (\ opts -> opts { optInteractive = True }))
     "interactive mode"
 , Option ['o']     ["output"]
     (ReqArg (\ f opts -> opts { optOutput = f }) "FILE")
     "specify output file name"
 , Option ['l']     ["loss"]
     (ReqArg (\ d opts -> opts { optLoss = read d}) "FLOAT")
     "specify battery loss of initial charge when discharging. Must be 0<loss<1"
 ]

compilerOpts :: [String] -> IO (Options, String)
compilerOpts argv =
   case getOpt RequireOrder options argv of
      (o,[n],[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,[]) -> ioError (userError (onlyOne ++ usageInfo header options))
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: program name [OPTION...] files...\n\n"
        onlyOne = "Supply only one input file name\n\n"


-- horrible hack function, to read date. Will crash with wrong input :)
readDate :: String -> Date
readDate ys = D day month year
  where day = read [head ys, ys !! 1]
        month = read [ ys !! 3, ys !! 4]
        year = read [ys !! 6, ys !! 7, ys !! 8, ys !! 9]

-- Parses a string to PriceData unit
readit :: String -> PriceData TimeStamp Price
readit [] = PD (TS (D 1 1 1900) "00-01") 0.0 -- Fail time
readit xs = PD (TS date time) price
  where line = words xs
        date = readDate $ head line
        time = line !! 1
        price = read (line !! 2 ) :: Price
        
lastHour :: PriceData TimeStamp Price -> Bool
lastHour (PD (TS _ time) _)
  | time == "23-00" = True
  | otherwise       = False

type Unit = PriceData TimeStamp Price

lastReq :: Reqs -> [Unit] -> Reqs
lastReq req [] = req
lastReq (Full loss _) (_:xs) = lastReq (Empty loss) xs
lastReq (Empty loss) (x:xs) = lastReq (Full loss $ value x) xs

giveNextD :: TimeStamp -> TimeStamp
giveNextD (TS (D d m y) _)
  | d == 28 && y `elem` [1996,2000..2096] = TS (D (d+1) m y) "14-15"
  | d == 28 && m == 2 = TS (D 1 3 y) "14-15"
  | d == 31 && m == 12 = TS (D 1 1 (y+1)) "14-15"
  | d == 31 && m `elem` [1,3,5,7,8,10] = TS (D 1 (m+1) y) "14-15"
  | d == 30 && m `elem` [2,4,6,9,11] = TS (D 1 (m+1) y) "14-15"
  | otherwise = TS (D (d+1) m y) "14-15"


cutTime :: TimeStamp -> [Unit] -> [Unit]
cutTime _ [] = []
cutTime ab@(TS (D d m y) hours) xs
  | hours == "14-15" || hours > "14-15"= takeWhile (nextDay . tag) xs
  | otherwise = takeWhile (today . tag) xs
  where nextDay time= time < giveNextD ab
        today time = time < TS (D d m y) "14-15"

accumulate :: (Reqs, [Unit],[Unit]) -> Unit -> (Reqs, [Unit], [Unit])
accumulate (a,b,c) d =
  if lastHour d then flush else (a,b,day)
  where day = c++[d]
        highs = localMax day
        lows = localMin day
        optimal = snd (bestMoney a highs lows)
        cut = cutTime (tag $ head c) optimal
        eqs (TS _ hours) = hours == "14-15"
        newA = lastReq a cut
        newB = b ++ cut
        newC = dropWhile (not . eqs . tag ) $ tail day
        flush = (newA, newB, newC)


nicePrint :: (Bool, String) -> Unit -> (Bool, String)
nicePrint (full, xs) (PD ts p)
  | full = (False, xs++"\nSell order " ++ niceTime ts ++ " Price: " ++ show p)
  | otherwise = (True, xs++"\nBuy order " ++ niceTime ts ++ " Price: "++ show p)
  where niceTime (TS (D d m y) h) = intercalate "-" [show d,show m,show y] ++ " " ++ h ++ "."


interactive :: IO()
interactive = do
  putStrLn "This program optimizes battery energy storage system."           
  putStr "Provide input file name: "
  infile <- getLine
  putStr "\nProvide output file name: "
  out <- getLine
  putStr "\nHow much of original charge is lost between charge and discharge (default 0.2): "
  xs <- getLine
  let loss = case reads xs :: [(Float,String)] of
        [] ->  0.2
        [(a,_)] -> a
  inph <- openFile infile ReadMode
  _ <- hGetLine inph            -- Disregarding header line
  all <- hGetContents inph
  writeOut loss infile out $ solve loss all
  hClose inph

writeOut :: ChargeLoss -> String -> String -> (Price, Int, String) -> IO()
writeOut loss infile outfile (profit, cycles, res) = do 
  outh <- openFile outfile WriteMode
  hPutStrLn outh "Output generated with helen-haskell program"
  hPutStrLn outh $ "Input file: " ++ show infile
  hPutStrLn outh $ "Loss of charge between charge and discharge: " ++ show loss
  hPutStrLn outh $ "Profit made " ++ show profit
  hPutStrLn outh $ "Total cycles " ++ show cycles
  hPutStrLn outh res  --(unlines $ map show resultList)
  putStrLn $ "Profit made: " ++ show profit ++
    ", cycles: " ++ show cycles ++ ". Loss : " ++ show loss
  hClose outh

solve :: ChargeLoss -> String -> (Price, Int, String)
solve loss all =   let allLines = map readit $ lines all
                       (a,b,c) = foldl accumulate (Full loss 0,[],[]) allLines
                       highs = localMax c
                       lows = localMin c
                       resultList = (++) b $ snd (bestMoney a highs lows)
                       profit = calcPrice (Full loss 0) resultList
                       cycles = length resultList `div` 2 +1
                       totalres = snd $ foldl nicePrint (True,"") resultList
                   in (profit, cycles, totalres)


mainFunc :: IO()
mainFunc = do
  argv <- getArgs
  (op,infile) <- compilerOpts argv
  when (optInteractive op) interactive
  when (not $ optInteractive op) $ do
    let out = optOutput op
        oLoss = optLoss op
        loss = if oLoss < 1 && oLoss > 0 then oLoss else 0.2
    inph <- openFile infile ReadMode
    _ <- hGetLine inph            -- Disregarding header line
    all <- hGetContents inph
    writeOut loss infile out $ solve loss all
    hClose inph
