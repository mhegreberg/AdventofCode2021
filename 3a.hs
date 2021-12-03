{-
Advent of Code
Mark Hegreberg
2a - calculate final position from submarine headings
-}

sampleData = [00100, 11110, 10110, 10111, 10101, 01111, 00111, 11100, 10000, 11001, 00010, 01010] :: [Integer]





main = do
   contents <- readFile "./3.input" 
   let input = lines contents
       input' = map (read::String->Int) input
   print $ sumPlaces input'



sumPlace :: (Foldable t, Integral a) => a -> t a -> a
sumPlace place = foldl
        (\acc x -> acc + 
        (x `mod` (place*10))
        `div` place) 0

sumPlaces :: (Foldable t, Integral a) => t a -> [a]
sumPlaces x =
        map ((`sumPlace` x) 
        . (10^))
        (reverse [0..12])

gamma :: (Integral a, Foldable t, Num b) => a -> t a -> [b]
gamma size x =
        map (gammaNormalize 
        . subtract (size `div` 2))
        (sumPlaces x)

gammaNormalize :: (Ord a, Num a, Num p) => a -> p
gammaNormalize x 
        | x > 0 = 1 
        | otherwise = 0

toDec :: (Num a, Integral a) => [a] -> a
toDec [] = 0
toDec [a] = a
toDec a = 2 ^ (toDec (init a)) + last a 

