-- tested using HUGS
checkNumber :: Int -> Bool
checkNumber a = if a*2 > 9 then True
  else False
luhnDouble a = if checkNumber a then a*2 - 9
	else a*2
luhnSumMod a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10
luhn a b c d | luhnSumMod a b c d == 0 = True
             | otherwise = False
