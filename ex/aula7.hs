rev :: [Int] -> [Int]
rev [] = []
rev (x : xs) = rev xs ++ [x]

(+++) :: [a] -> [a] -> [a]
(+++) x [] = x
(+++) [] x = x
(+++) (x : xs) y = x : (+++) xs y
