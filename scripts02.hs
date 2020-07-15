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
