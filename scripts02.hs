-- script encodes a string into binaries with parity check, transmits, 
-- and then decodes the binaries into the original string
-- transmit "The quick brown fox jumps over the lazy dog."
-- transmiterror "The quick brown fox jumps over the lazy dog."
import Data.Char
type Bit = Int
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
  where weights = iterate (*2) 1
-- bin2int = foldr (\x y -> x + 2*y) 0
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)
-- default parity :: Num a => [a] -> Int
parity :: [Bit] -> Int
parity bits = length [x | x <- bits, x == 1] `mod` 2
make9 :: [Bit] -> [Bit]
make9 bits = take 9 ([parity bits] ++ bits ++ repeat 0)
-- ord converts char to int ascii
encode :: String -> [Bit]
encode = concat . map (make9 . int2bin . ord)
chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)
-- default check :: [Int] -> [Int]
check :: [Bit] -> [Bit]
check bits | length [x | x <- tail bits, x == 1] 
               `mod` 2 == head bits = tail bits
           | otherwise = error "Parity Error"
-- chr converts int ascii to char
decode :: [Bit] -> String
decode = map (chr . bin2int . check) . chop9
transmit :: String -> String
transmit = decode . channel . encode
channel :: [Bit] -> [Bit]
channel = id
transmiterror = decode . tail . channel . encode
-----------------------------------------
-- 2016 Programming in Haskell, chapter 8, question 8
-- check for tautology: isTaut p6
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equal Prop Prop
type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)
-- | A function that evaluates a proposition 
-- given a substitution for its variables
-- defined by pattern matching on the seven 
-- possible forms that the proposition can
-- have.
eval :: Subst -> Prop -> Bool
-- | The value of a constant proposition is 
-- simply the constant itself.
eval _ (Const b)   = b
-- | The value of a variable is obtained by 
-- looking up its value in the substitution.
eval s (Var x)     = find x s
-- | The value of a conjunction is given by 
-- taking the conjunction of the values of
-- the two argument propositions.
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
-- | The value of an implication is obtained 
-- by the `<=` ordering on logical values.
eval s (Imply p q) = eval s p <= eval s q
-- logical disjunction
eval s (Or p q) = eval s p || eval s q
-- equivalence
eval s (Equal p q) = eval s p == eval s q
-- | A function that returns a list of all 
-- the variables in a proposition.
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equal p q) = vars p ++ vars q
-- | A function that produces a list of 
-- logical values of a given length.
bools :: Int -> [[Bool]]
bools 0 = [[]]
-- | Append the results of taking two copies 
-- of the recursively produced lists
bools n = nope bss ++ yes bss
  where 
    -- | Place `False` in front of each list 
    -- in the first copy.
    nope = map (False:)
    -- | Place `True` in front of each list 
    -- in the second copy.
    yes = map (True:)
    bss = bools (n-1)
-- | A function that generates all possible 
-- substitutions for a proposition.
substs :: Prop -> [Subst]
-- | Zipping the list of variables with each 
-- of the resulting lists.
substs p = map (zip vs) (bools (length vs))
  where
    -- | Extracting the variables and removing 
    -- duplicates from the list.
    vs = rmdups $ vars p
-- | A function that decides if a proposition 
-- is a tautology.
isTaut :: Prop -> Bool
-- | Check if it evaluates to `True` for all 
-- possible substitutions.
isTaut p = and [eval s p | s <- substs p]
-- Proposition variables
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply
        (Var 'A') (Var 'B'))) (Var 'B')
p5 = Or (Var 'A') (Var 'B')
p6 = Equal (Var 'A') (Var 'B')
-----------------------------------------
-- 2016 Programming in Haskell, chapter 8, question 9
-- value (Mult (Val 2) (Add (Val 2) (Val 3))) 
-- Calculating:
-- eval (Mult (Val 2) (Add (Val 2) (Val 3))) []
-- eval (Val 2) (EVAL2 (Add (Val 2) (Val 3)) : [])
-- exec (EVAL2 (Add (Val 2) (Val 3)) : []) 2
-- eval (Add (Val 2) (Val 3)) (MULT 2 : [])
-- eval (Val 2) (EVAL1 (Val 3) : (MULT 2 : []))
-- exec (EVAL1 (Val 3) : (MULT 2 : [])) 2
-- eval (Val 3) (ADD 2 : (MULT 2 : []))
-- exec (ADD 2 : (MULT 2 : [])) 3
-- exec (MULT 2 : []) (2+3)
-- exec [] (2*(2+3))
-- (2*(2+3))
data Expr = Val Int 
          | Add Expr Expr 
          | Mult Expr Expr 
          deriving Show
type Cont = [Op] 
data Op = EVAL1 Expr 
        | EVAL2 Expr
        | ADD Int 
        | MULT Int
        deriving Show
eval2 :: Expr -> Cont -> Int
eval2 (Val n) c = exec c n
eval2 (Add x y) c = eval2 x (EVAL1 y : c)
eval2 (Mult x y) c = eval2 x (EVAL2 y : c)
exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL1 y : c) n = eval2 y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)
exec (EVAL2 y : c) n = eval2 y (MULT n : c)
exec (MULT n : c) m = exec c (n*m)
value :: Expr -> Int
value e = eval2 e []