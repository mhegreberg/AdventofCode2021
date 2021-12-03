{-
Advent of Code
Mark Hegreberg
2a - calculate final position from submarine headings
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
   print $ solution 12 input'


solution width x = 
    let gam = gamma width x
        eps = epsilon gam
    in  toDec gam * toDec eps

sumPlace :: (Foldable t, Integral a) => a -> t a -> a
sumPlace place = foldl
        (\acc x -> acc +
        (x `mod` (place*10))
        `div` place) 0

--sumPlaces :: (Foldable t, Integral a) => t a -> [a]
sumPlaces width x =
        map ((`sumPlace` x) . (10^)) (reverse [0..(width-1)])

--gamma :: (Integral a, Foldable t, Num b) => a -> t a -> [b]
gamma width x =
    let size = length x
    in  map (gammaNormalize
        . subtract (size `div` 2))
        (sumPlaces width x)

gammaNormalize :: (Ord a, Num a, Num p) => a -> p
gammaNormalize x
        | x > 0 = 1
        | otherwise = 0


epsilon :: (Num a, Eq a) => [a] -> [a]
epsilon [] = []
epsilon a
        | x == 1 = 0:epsilon (tail a)
        | x == 0 = 1:epsilon (tail a)
    where
      x = head a




toDec :: [Integer] -> Integer
toDec input = foldr (\x y ->x + 2*y) 0 $ reverse input
