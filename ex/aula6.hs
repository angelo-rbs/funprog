module Listas where

data ListInt
  = PairListInt Int ListInt
  | SingleListInt Int
  | EmptyListInt
  deriving (Show, Eq)

pos :: ListInt -> Int -> Int
pos (SingleListInt a) n
  | n == 0 = a
  | otherwise = error "índice inexistente"
pos (PairListInt a (PairListInt b c)) n
  | n == 0 = a
  | n == 1 = b
  | n > 1 = pos (PairListInt b c) (n -1)
  | otherwise = error "índice inexistente"
pos (PairListInt a (SingleListInt b)) n
  | n == 0 = a
  | n == 1 = b
  | otherwise = error "índice inexistente"
pos (PairListInt a EmptyListInt) n
  | n == 0 = a
  | otherwise = error "índice inexistente"
pos EmptyListInt n = error "índice inexistente"

head :: ListInt -> Int
head (PairListInt a _) = a
head (SingleListInt a) = a
head EmptyListInt = error "lista vazia"

tail :: ListInt -> ListInt
tail (PairListInt a b) = b
tail (SingleListInt a) = EmptyListInt
tail EmptyListInt = error "lista vazia"

data List t = PairList t (List t) | SingleList t | EmptyList
  deriving (Show, Eq)

hd :: List t -> t
hd (PairList a b) = a
hd (SingleList a) = a
hd EmptyList = error "lista vazia"

tl :: List t -> List t
tl (PairList a b) = b
tl (SingleList a) = EmptyList
tl EmptyList = error "lista vazia"
