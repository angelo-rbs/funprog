{- In a similar way to && in section 4.4, show how the disjunction operator or
can be deﬁned in four diﬀerent ways using pattern matching. -}

(#) :: Bool -> Bool -> Bool
False # False = False
_ # _ = True