import System.IO  
import Control.Monad
import Data.List as L
import Data.Map as M

main = do  
    contents <- readFile "input"
    let 
        values = L.map read (words contents)
        result1 = solve1 values
        result2 = solve2 values
    putStr (show result1)
    putStr ("\n")
    putStr (show result2)

solve1 :: [Int] -> Int
solve1 = sum . L.map (abs . uncurry (-)) . uncurry zip . (\(xs, ys) -> ((sort xs), (sort ys))) . group ([], [])
    where
        group gs [] = gs
        group (xs, ys) (x:y:xys) = group ((x:xs), (y:ys)) xys


solve2 :: [Int] -> Int
solve2 = (\(xs, counts) -> sum (L.map (\x -> x * (findWithDefault 0 x counts)) xs)) . groupAndCount ([], empty)
    where
        groupAndCount :: ([Int], Map Int Int) -> [Int] -> ([Int], Map Int Int)
        groupAndCount gs [] = gs
        groupAndCount (xs, counts) (x:y:xs') = groupAndCount (x:xs, insertWith (+) y 1 counts) xs'

