{- Do the same for the following alternative deﬁnition, and note the diﬀerence
in the number of conditional expressions that are required:
True && b = b
False && _ = False -}

(#) :: Bool -> Bool -> Bool
(#) a b = if a then b else False