
pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x, y, z) | z <- [1..n], y <- [1..z], x <- [1..z], x^2 + y^2 == z^2]