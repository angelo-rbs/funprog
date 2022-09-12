module ExNat where

-- Do not alter this import!
import Prelude
  ( Bool (..),
    Eq (..),
    Integral,
    Num (..),
    Ord (..),
    Show (..),
    error,
    not,
    otherwise,
    undefined,
    ($),
    (&&),
    (++),
    (.),
    (||),
  )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where
  -- zero  should be shown as O
  -- three should be shown as SSSO
  show Zero = "O"
  show (Succ n) = "S" ++ show n

instance Eq Nat where
  (==) Zero (Succ n) = False
  (==) (Succ n) Zero = False
  (==) Zero Zero = True
  (==) (Succ n) (Succ m) = (==) n m

instance Ord Nat where
  (<=) Zero _ = True
  (<=) (Succ n) Zero = False
  (<=) (Succ n) (Succ m) = (<=) n m

  -- Ord does not REQUIRE defining min and max.
  -- Howevener, you should define them WITHOUT using (<=).
  -- Both are binary functions: max m n = ..., etc.

  min Zero _ = Zero
  min _ Zero = Zero
  min (Succ m) (Succ n) = min m n

  max Zero n = n
  max n Zero = n
  max (Succ m) (Succ n) = max m n

isZero :: Nat -> Bool
isZero n = n == Zero

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

even :: Nat -> Bool
even Zero = True
even (Succ n) = not (even n)

odd :: Nat -> Bool
odd Zero = False
odd (Succ n) = not (odd n)

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) Zero x = x
(<+>) x Zero = x
(<+>) (Succ n) (Succ m) = Succ (Succ (n + m))

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) Zero _ = Zero
(<->) a Zero = a
(<->) (Succ m) (Succ n) = (<->) m n

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) Zero _ = Zero
(<*>) _ Zero = Zero
(<*>) (Succ m) n = (m <*> n) + n

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) Zero Zero = error "indeterminação"
(<^>) Zero _ = Zero
(<^>) _ Zero = Succ Zero
(<^>) m (Succ Zero) = m
(<^>) m (Succ n) = (<^>) m n <*> m

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) _ Zero = error "divisão por zero"
(</>) Zero _ = Zero
(</>) m (Succ Zero) = m
(</>) m n = (</>) (m <-> n) n <+> Succ Zero

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) _ Zero = error "divisão por zero"
(<%>) Zero _ = Zero
(<%>) m n
  | n <= m = (<%>) (m <-> n) n
  | otherwise = m

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) m n = (<%>) m n == Zero

divides = (<|>)

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff Zero x = x
absDiff x Zero = x
absDiff (Succ x) (Succ y)
  | x == y = Zero
  | y <= x = absDiff x y
  | otherwise = absDiff (Succ y) (Succ x)

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero = Succ Zero
factorial (Succ Zero) = Succ Zero
factorial (Succ n) = Succ n <*> factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg m
  | (==) m Zero = Zero
  | (<=) m Zero = -1
  | otherwise = Succ Zero

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined

--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined

-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where
  (+) = (<+>)
  (*) = (<*>)
  (-) = (<->)
  abs n = n
  signum = sg
  fromInteger x
    | x < 0 = undefined
    | x == 0 = undefined
    | otherwise = undefined
