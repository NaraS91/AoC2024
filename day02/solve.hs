import System.IO  
import Control.Monad
import Data.List

main = do  
    contents <- readFile "input"
    let 
        values :: [[Int]]
        values = (map (map read . words) . lines) contents
        result1 = solve1 values
        result2 = solve2 values
    putStr (show result1)
    putStr ("\n")
    putStr (show result2)

solve1 :: [[Int]] -> Int
solve1 = length . filter id . map listSolver

listSolver :: [Int] -> Bool
listSolver (x:x':xs) = (all (\i -> abs i <= 3 && abs i > 0 && i * unit > 0) . map (uncurry (-)) . zip (x:x':xs)) (x':xs)
    where
        unit = x - x'

solve2 :: [[Int]] -> Int
solve2 = length . filter id . map internalSolver
    where
        internalSolver :: [Int] -> Bool
        internalSolver xss@(x:x':xs)
            | wrong_diffs > 2 = False
            | wrong_diffs > 0 = listSolver (h1 ++ rest) || listSolver (h1 ++ (wrongElem : tail rest))
            | plus == 0 || minus == 0 = True
            | plus > 2 && minus > 2 = False
            | plus > 2 = if h2 == [] then listSolver rest2 else listSolver (h2 ++ rest2) || listSolver (h2 ++ (decElem : tail rest2))
            | otherwise = if h3 == [] then listSolver rest3 else listSolver (h3 ++ rest3) || listSolver (h3 ++ (incElem : tail rest3))
            where
                -- can be made more efficient... but no
                (plus, minus) = foldl (\(p, m) i -> if i > 0 then (p+1, m) else (p, m+1)) (0, 0) diffs
                wrong_diffs = length (filter (\i -> i == 0 || abs i > 3) diffs)
                diffs  = map (uncurry (-)) (zip (x:x':xs) (x':xs))

                Just ind = findIndex (\i -> i == 0 || abs i > 3) diffs
                (h1, wrongElem:rest) = splitAt ind xss

                Just decInd = findIndex (\i ->  i < 0) diffs
                (h2, decElem:rest2) = splitAt decInd xss

                Just incInd = findIndex (\i -> i > 0) diffs
                (h3, incElem:rest3) = splitAt incInd xss