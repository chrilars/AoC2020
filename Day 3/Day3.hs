count ::  Int -> Int -> Int -> Int -> [String] -> Int
count _   trees _     _     [] = trees
count col trees stepx stepy i  | isTree      = count newCol (trees + 1) stepx stepy newList
                               | otherwise   = count newCol trees stepx stepy newList
    where isTree  = head i !! col == '#'
          newList = drop stepy i
          newCol  = (col + stepx) `mod` 31

sumCount :: [String] -> Int
sumCount i = c 1 1 * c 3 1 * c 5 1 * c 7 1 * c 1 2
    where c x y = count 0 0 x y i

main :: IO()
main = print . sumCount . lines =<< readFile "input.txt"
