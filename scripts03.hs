-- Haskell Programming from first principles
-- Chapter 11.09 Intermission: Jammin Exercises
module Jammin where
  import Data.List
  data Fruit =
    Peach
    | Plum
    | Apple
    | Blackberry
    deriving (Eq, Ord, Show)
  data JamJars =
    Jam { fruit :: Fruit
    , jars :: Int }
    deriving (Eq, Ord, Show)
  row1 = Jam Peach 1
  row2 = Jam Plum 1
  row3 = Jam Apple 10
  row4 = Jam Blackberry 1
  row5 = Jam Peach 1
  row6 = Jam Peach 10
  allJam = [row1, row2, row3, row4, row5, row6]
  allJars = map jars allJam
  totalJars = sum allJars
  mostRow = foldr1 (\x y ->if jars x >= jars y then x else y) allJam
  -- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
  -- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
  compareKind (Jam k _) (Jam k' _) = compare k k'
  sortKinds = sortBy compareKind allJam
  sameKind (Jam k _) (Jam k' _) = k == k'
  groupJam = groupBy sameKind sortKinds
