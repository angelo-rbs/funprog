
{- Show how the meaning of the following curried function deﬁnition can be
formalised in terms of lambda expressions:
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z -}

mult :: Int -> (Int -> (Int -> Int))
mult = \z -> (\y -> (\x -> x*y*z))

