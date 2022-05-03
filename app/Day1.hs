module Day1 where

solver1 :: [Int] -> Maybe Int
solver1 [] = Nothing
solver1 (i:is)
    | length k == 1 = Just (i * head k)
    | otherwise = solver1 is
    where k = filter (\x -> i + x == 2020) is

solver2 :: [Int] -> Maybe Int
solver2 [] = Nothing
solver2 input
    = Just (head [ x*y*z | x <- input, y <- input, z <- input, x + y + z == 2020 ])
