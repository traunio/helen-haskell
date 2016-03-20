-- Copyright (c) 2016 Tapani Raunio, tapani.raunio@gmail.com

import Helen_skaba
import System.IO
import Data.List

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


main :: IO()
main = do
  putStrLn "Running optimisation"
  inph <- openFile "elspot_test.txt" ReadMode
  _ <- hGetLine inph
  sndStr <- hGetContents inph
  let allLines = map readit $ lines sndStr
      (a,b,c) = foldl accumulate (Full 0.2 0,[],[]) allLines
      highs = localMax c
      lows = localMin c
      resultList = (++) b $ snd (bestMoney a highs lows)
      profit = calcPrice (Full 0.2 0) resultList
      cycles = length resultList `div` 2 +1
      totalres = snd $ foldl nicePrint (True,"") resultList


  outh <- openFile "results.txt" WriteMode
  hPutStrLn outh $ "Profit made " ++ show profit
  hPutStrLn outh $ "Total cycles " ++ show cycles
  hPutStrLn outh totalres --(unlines $ map show resultList)


  hClose inph
  hClose outh
