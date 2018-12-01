import qualified Data.Set as Set

parse :: [String] -> [Int]
parse = map (read . dropWhile (== '+'))

solve01 :: [Int] -> Int
solve01 = foldl (+) 0

solve02 :: [Int] -> Int
solve02 input = solve02f input 0 input Set.empty

solve02f :: [Int] -> Int -> [Int] -> Set.Set Int -> Int
solve02f input current l s
  | Set.member current s = current
  | otherwise = case l of
      (x:xs) -> solve02f input (current + x) xs (Set.insert current s)
      otherwise -> solve02f input current input s

solve = do
  contents <- readFile "../resources/input-01.data"
  let input = parse (lines contents)
  let part1 = solve01 input
  let part2 = solve02 input
  return (part1, part2)
