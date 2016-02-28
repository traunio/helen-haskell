-- Copyright (c) 2016 Tapani Raunio, tapani.raunio@gmail.com

import Helen_skaba
import System.IO

-- horrible hack function, brrr....
readDate :: String -> Date
readDate ys = D day month year
  where day = read [head ys, ys !! 1]
        month = read [ ys !! 3, ys !! 4]
        year = read [ys !! 6, ys !! 7, ys !! 8, ys !! 9]

readit :: String -> String
readit [] = []
readit xs = show (PD (TS date time) price)
  where line = words xs
        date = readDate $ head line
        time = line !! 1
        price = line !! 2


main :: IO()
main = do
  putStrLn "Greetings, yeehaa"
  inph <- openFile "elspot_2014_fi.txt" ReadMode
  inpStr <- hGetLine inph
  putStrLn $ "First line: " ++ inpStr
  sndStr <- hGetLine inph
  let result = readit sndStr
  putStrLn $ "Second line: " ++ result

  hClose inph
