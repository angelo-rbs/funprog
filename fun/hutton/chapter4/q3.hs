{- Consider a function safetail :: [a] -> [a] that behaves in the same way
as tail except that it maps the empty list to itself rather than producing an
error. Using tail and the function null :: [a] -> Bool that decides if a
list is empty or not, deﬁne safetail using:
a. a conditional expression;
b. guarded equations;
c. pattern matching. -}

safetail'a :: [a] -> [a]
safetail'a xs = if null xs then [] else tail xs

safetail'b :: [a] -> [a]
safetail'b xs | null xs = []
              | otherwise = tail xs

safetail'c :: [a] -> [a]
safetail'c [] = []
safetail'c (_:a) = a