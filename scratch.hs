
tellLate :: (Num a, Ord a) => a -> String
tellLate time
        | time  <  0 = "You're actually early"
        | time ==  0 = "You're on time!"
        | time <= 15 = "Basically on time"
        | time <= 30 = "Traffic?"
        | otherwise  = "Wow, you're actually late"

fibbonacci :: (Integral a) =>a -> a
fibbonacci 0 = 1
fibbonacci 1 = 1
fibbonacci n = fibbonacci (n-1) + fibbonacci (n-2)


isIncreased :: (Ord a) => [a] -> Ordering
isIncreased [] = EQ
isIncreased [_] = EQ
isIncreased [x, y] = compare y x


increaseCheck :: (Ord a) => [a] ->[Ordering]
increaseCheck [] = [EQ]
increaseCheck [_] = [EQ]
increaseCheck [x, y] = [compare y x]
increaseCheck (x:y:ys) = compare y x:increaseCheck (y:ys)
