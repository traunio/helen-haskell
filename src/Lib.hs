module Lib (mainfunction) where

import Helen_skaba
import System.IO
import Data.List
import System.Console.GetOpt
import Control.Monad (when)
import System.Environment


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


mainfunction :: IO()
mainfunction = do
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
