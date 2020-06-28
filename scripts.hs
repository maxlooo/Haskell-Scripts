-- tested using HUGS
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

-- find all prime number factors for n
-- test is for creating a product of primes
-- example: factors (test 10) == primes 10
factor n = [x | x <- [1..n], n `mod` x == 0]
primes n = [x | x <- [1..n], factor x == [1,x]]
factors n = [x | x <- primes n, n `mod` x == 0]
test n = product (primes n)
