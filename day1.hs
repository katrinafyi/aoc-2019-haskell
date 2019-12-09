import Data.Char

fuel m = div m 3 - 2


fuel' m = 
    if f <= 0 then 0 else f + fuel'(f)
    where f = fuel(m)

solve1 x =
    sum $ map fuel x

solve2 x = 
    sum $ map fuel' x

parse :: [[Char]] -> [Int]
parse masses =
    map read masses

main = do
    text <- getContents
    print $ solve1 $ parse $ lines text
    print $ solve2 $ parse $ lines text

