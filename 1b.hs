{-
Advent of Code
Mark Hegreberg
1b - find number of times depth increases, but on a 3 sum sliding window
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

increaseCheck :: (Ord a, Num a) => [a] ->[Ordering]
increaseCheck [] = [EQ]
increaseCheck [_] = [EQ]
increaseCheck [x, y] = [EQ]
increaseCheck [x,y,z] = [EQ]
--increaseCheck (x:y:ys) = compare y x:increaseCheck (y:ys)
increaseCheck (a:b:c:d:ys) = compare (b+c+d) (a+b+c):increaseCheck (b:c:d:ys)
