{- Deﬁne a function third :: [a] -> a that returns the third element in a list
that contains at least this many elements using:
a. head and tail;
b. list indexing !!;
c. pattern matching. -}

third'a :: [a] -> a
third'a xs = head (tail (tail xs))

third'b :: [a] -> a
third'b  xs = xs !! 2

third'c :: [a] -> a
third'c [_,_,b] = b
third'c (_:_:b:_) = b