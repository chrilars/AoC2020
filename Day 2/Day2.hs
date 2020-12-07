type Line = (Int, Int, Char, String)

parse :: String -> Line
parse x = (l, h, head char, pass)
    where [range, char, pass] = words x
          [l,h] = map read [takeWhile (/='-') range, tail $ dropWhile (/='-') range]

solve :: Line -> Bool
solve (l, h, c, p) = (f == c) /= (s == c)
    where f = p !! (l-1)
          s = p !! (h-1)

main :: IO ()
main = print . length . filter solve . map parse . lines =<< readFile "input.txt"
