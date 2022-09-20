factors :: Int -> [Int]
factors n = [ f | f <- [1..n], n `mod` f == 0]


perfects :: Int -> [Int]
perfects x = [ p | p <- [1..x],  sum (init (factors p)) == p]