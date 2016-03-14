-- Copyright (c) 2016 Tapani Raunio, tapani.raunio@gmail.com

import Helen_skaba
import System.IO

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

-- -- Helper function to
-- lastHour :: PriceData TimeStamp Price -> Bool
-- lastHour (PD (TS _ time) _)
--   | time == "23-00" = True
--   | otherwise       = False


main :: IO()
main = do
  putStrLn "Greetings, yeehaa"
  inph <- openFile "elspot_test.txt" ReadMode
  inpStr <- hGetLine inph
  putStrLn $ "First line: " ++ inpStr
  sndStr <- hGetContents inph
  let allLines = map readit $ lines sndStr
      highs = localMax allLines
      lows = localMin allLines
      result = bestMoney (Full 0.2 0) highs lows


  putStrLn $ "Best result " ++ show (fst result) ++ " e."
  putStrLn $ unlines $ map show (snd result)
  -- putStrLn "All the highs:"
  -- putStrLn $ unlines $ map show highs
  --
  -- putStrLn "All the lows:"
  -- putStrLn $ unlines $ map show lows

  hClose inph
