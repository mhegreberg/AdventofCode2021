{-
Advent of Code
Mark Hegreberg
1a - find number of times depth increases
-}

sampleData = [199,200,208,210,200,207,240,269,260,263]

inputData = readFile "./1.input"


main = do
       contents <-readFile "./1.input"
       let input = lines contents
           input' = map (read::String->Int) input
           answer = solution input'
       print answer


solution :: (Ord a, Num a) => [a] -> Int
solution input = length [x | x <- increaseCheck input, x == GT]

increaseCheck :: (Ord a) => [a] ->[Ordering]
increaseCheck [] = [EQ]
increaseCheck [_] = [EQ]
increaseCheck [x, y] = [compare y x]
increaseCheck (x:y:ys) = compare y x:increaseCheck (y:ys)
