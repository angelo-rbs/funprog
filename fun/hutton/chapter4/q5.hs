
{- Without using any other library functions or operators, show how the meaning
of the following pattern matching deﬁnition for logical conjunction && can be
formalised using conditional expressions:
True && True = True
_ && _ = False -}

(#) :: Bool -> Bool -> Bool
(#) a b = if a then (if b then True else False) else False