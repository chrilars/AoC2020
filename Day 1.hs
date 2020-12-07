solve :: [Int] -> Int
solve x = head [a*b*c | a <- x, b <- x, c <- x, a+b+c == 2020]

main :: IO ()
main = print . solve . map read . lines =<< readFile "input.txt"
            