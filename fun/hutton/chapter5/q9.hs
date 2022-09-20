
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ (xs!!pos)*(ys!!pos) | pos <- [0..n-1] ]
    where n = min (length xs) (length ys)