-- tested using HUGS
-- import Data.Char for Caesar Cipher
import Data.Char
---------------------------------------
-- for checking the validity of 4 digit number
-- examples: luhn 1 7 8 4
-- True
-- luhn 4 7 8 3
-- False
checkNumber :: Int -> Bool
checkNumber a = if a*2 > 9 then True
  else False
luhnDouble a = if checkNumber a then a*2 - 9
  else a*2
luhnSumMod a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10
luhn a b c d | luhnSumMod a b c d == 0 = True
             | otherwise = False
---------------------------------------
-- find all prime number factors for n
-- test is for creating a product of primes
-- example: factors (test 10) == primes 10
-- factorlist includes all repeated prime factors
-- example: factorlist 1260
-- [2,2,3,3,5,7]
factor n = [x | x <- [1..n], n `mod` x == 0]
primes n = [x | x <- [1..n], factor x == [1,x]]
factors n = [x | x <- primes n, n `mod` x == 0]
test n = product (primes n)
safehead list | null list = 1000
              | otherwise = head list
safetail list | null list = []
              | otherwise = tail list
factorlist :: Int -> [Int]
factorlist n | n <= 0 = []
             | n == 1 = [1]
             | otherwise = recurse ([n `div` safehead(factors n)], 
               factors n, [] ++ [safehead(factors n)])
recurse ([x], ys, zs) | x == 1 = zs
                    | x `mod` safehead ys==0 
                      = recurse ([x `div` safehead ys], 
                        ys, zs ++ [safehead ys])
                    | otherwise = recurse ([x], safetail ys, zs)
---------------------------------------
-- encode and decode strings using Caesar Cipher
-- example: encode 3 "The quick brown fox jumps over the lazy dog."
-- crack (encode 3 "The quick brown fox jumps over the lazy dog.")
let2int :: Char -> Int
let2int c = ord c - ord 'a'
upplet2int c = ord c - ord 'A'
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)
int2upplet n = chr (ord 'A' + n)
shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2upplet ((upplet2int c + n) `mod` 26)
  | otherwise = c
encode :: Int -> [Char] -> [Char]
encode n xs = [shift n x | x <- xs]
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]
-- default inference percent :: (Integral a, Integral b, Fractional c) => a -> b -> c
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100
-- default count :: Eq a => a -> [a] -> Int
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
-- default lowers :: [Char] -> Int
lowers :: String -> Int
lowers xs = length [x | x <- xs, x>='a' && x<='z']
-- default freqs :: [Char] -> [Float]
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs
-- default chisqr :: Fractional a => [a] -> [a] -> a
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']
-- default crack :: [Char] -> [Char]
stringToLower :: String -> String
stringToLower xs = [toLower x | x <- xs]
crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs (stringToLower xs)
---------------------------------------
-- using halve and merge to sort a list
-- example: msort [2,1,5,3,6,4,9,2,3]
-- [1,2,2,3,3,4,5,6,9]
-- example: msort "The quick brown fox jumps over the lazy dog."
-- "        .Tabcdeeefghhijklmnoooopqrrstuuvwxyz"
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x > y = merge (y:x:xs) ys
                    | x <= y = x : merge (xs) (y:ys)
halve list = (take (length list `div` 2) list, 
  drop (length list `div` 2) list)
msort :: Ord a => [a] -> [a]
msort list | null list = []
           | tail list == [] = list
           | otherwise = msort( fst (halve list)) 
              `merge` msort( snd (halve list))


