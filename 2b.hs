{-
Advent of Code
Mark Hegreberg
2b - calculate final position from submarine headings, where depth is determined by aim
-}
import Data.Char

sampleData = [("forward", 5), ("down", 5), ("forward", 8), ("up", 3), ("down", 8), ("forward", 2)]


readH a = init $ init a
readM a = [last a] 

main = do
   contents <- readFile "./2.input"
   let input = lines contents
       inputH = map readH input
       inputM = map readM input 
       inputM' = map (read::String->Int) inputM
       input' = zip inputH inputM'
   print $ solution input'


solution :: Num a => [(String, a)] -> a
solution a = horSum a * vertSum a 0

horSum :: (Num a) => [(String, a)] -> a
horSum [] = 0
horSum x
    | fst (head x) == "forward"= snd (head x) + horSum (tail x)
    | otherwise = horSum (tail x)

vertSum :: (Num a) => [(String, a)] -> a -> a
vertSum [] _ = 0
vertSum y aim
    | fst (head y) == "up" = vertSum (tail y) (aim - snd (head y))
    | fst (head y) == "down" = vertSum (tail y) (aim + snd (head y)) 
    | fst (head y) == "forward" = (aim * snd (head y)) + vertSum (tail y) aim
    | otherwise = vertSum (tail y) aim



