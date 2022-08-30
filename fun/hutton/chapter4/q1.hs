{- Using library functions, deﬁne a function halve :: [a] -> ([a],[a]) that
splits an even-lengthed list into two halves -}

halve :: [a] -> ([a], [a])
halve xs
  | even (length xs) = (take l xs, drop l xs)
  | otherwise = ([], [])
  where
    l = length xs `div` 2
