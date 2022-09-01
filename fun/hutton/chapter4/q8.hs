{- The Luhn algorithm is used to check bank card numbers for simple errors
such as mistyping a digit... -}

luhnDouble :: Int -> Int
luhnDouble x = if p > 9 then p - 9 else p
    where p = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum [luhnDouble(a), b, luhnDouble(c), d] `mod` 10 == 0