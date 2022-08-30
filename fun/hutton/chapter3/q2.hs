
bools :: [Bool]
bools = [False, True]

nums :: [[Int]]
nums = [[0, 2, 4, 6, 8, 10], [1, 3, 5, 7, 9]]

add :: Int -> Int -> Int -> Int
add x y z = x*(y+z) + y*(x+z) + z*(x+y)

copy :: a -> (a, a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply f x = (f) x
