module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C
import Distribution.Simple.Utils (xargs)
import Distribution.SPDX (LicenseId(ZPL_1_1))

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head [] = error "vazio"
head (x:xs) = x

tail :: [a] -> [a]
tail [] = error "empty list"
tail [x] = []
tail (x:xs) = xs 

--null :: [a] -> Bool


length :: Integral i => [a] -> i
length [] = 0
length (x:xs) = length xs + 1


sum :: Num a => [a] -> a
sum [] = error "empty list"
sum [x] = x
sum (x:xs) = x + sum xs 

product :: Num a => [a] -> a
product [] = error "empty list"
product [x] = x
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = append x (reverse xs)
    where append y [] = [y]
          append y (z:zs) = z : append y zs

(++) :: [a] -> [a] -> [a]
(++) [] xs = xs 
(++) xs [] = xs
(++) xs (y:ys) = (++) (append y xs) ys
    where append y [] = [y]
          append y (z:zs) = z : append y zs

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc y [] = [y]
snoc y (z:zs) = z : snoc y zs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "empty list"
minimum [x] = x
minimum (x:xs) = min x (minimum xs)
    where min a b
            | a < b = a 
            | otherwise = b

maximum :: Ord a => [a] -> a
maximum [] = error "empty list"
maximum [x] = x
maximum (x:xs) = max x (maximum xs)
    where min a b
            | a > b = a 
            | otherwise = b

-- take
-- drop

-- takeWhile
-- dropWhile

-- tails
-- init
-- inits

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

