{-
Advent of Code
Mark Hegreberg
3b - gather diagnostic data about O2 and CO2 from binary data
-}

sampleData = [00100,
              11110,
              10110,
              10111,
              10101,
              01111,
              00111,
              11100,
              10000,
              11001,
              00010,
              01010]
              :: [Int]





main = do
   contents <- readFile "./3.input"
   let input = lines contents
       input' = map (read::String->Int) input
   print $ solution 11 input'


solution width x = toDec' (filterOx width x) * toDec' (filterCO2 width x)
filterOx :: Int -> [Int] -> [Int]
filterOx 0 x = x
filterOx _ [x] = [x]
filterOx width x
        | com >= 0   = filterOx (width-1) (filter (isPlace place 1) x)
        | otherwise = filterOx (width-1) (filter (isPlace place 0) x)
        where
              place = 10 ^ (width-1)
              com = common $ getPlaces place x

filterCO2 :: Int -> [Int] -> [Int]
filterCO2 0 x = x
filterCO2 _ [x] = [x]
filterCO2 width x
        | com >= 0   = filterCO2 (width-1) (filter (isPlace place 0) x)
        | otherwise = filterCO2 (width-1) (filter (isPlace place 1) x)
        where
              place = 10 ^ (width-1)
              com = common $ getPlaces place x

common :: [Int] -> Int
common [] = 0
common [a] = a
common (x:xs)
        | x == 1 =  common xs + 1
        | x == 0 =  common xs - 1

getPlace :: (Eq a, Num a, Integral a) => a -> a -> a
getPlace place x
        | place == 1 = mod (x - mod x place) place'
        | otherwise  = mod (x - mod x place) place' `div` place
    where
      place' = place * 10

getPlaces :: (Integral a) => a -> [a] -> [a]
getPlaces place = map (getPlace place)

isPlace place val x
        | getPlace place x == val = True
        | otherwise = False


toDec :: Integral a => a -> a
toDec 0 = 0
toDec x = 2 * toDec (div x 10) + mod x 10

toDec' :: Integral a => [a] -> a
toDec' [x] = toDec x
