module Day2 where
import Data.List.Split

data PasswordRule = PasswordRule
  { minOcc :: Int,
    maxOcc :: Int,
    letter :: Char
  }

solver1 :: [String] -> Maybe Int
solver1 [] = Nothing
solver1 input = Just (length $ filter checkLine input)

checkLine :: String -> Bool
checkLine l = checkPassword (parseRule l) (last $ words l)

checkPassword :: PasswordRule -> String -> Bool
checkPassword (PasswordRule minOcc maxOcc letter) p = (minOcc <= occurances) && (occurances <= maxOcc)
  where
    occurances = length (filter (== letter) p)

parseRule :: String -> PasswordRule
parseRule input = PasswordRule minOcc maxOcc letter
  where
    ws = words input -- ws on form `["1-3","a:","abcde"]`
    interval = splitOn "-" $ head ws -- ["1","3"]
    minOcc = read $ head interval -- 1
    maxOcc = read $ last interval -- 3
    letter = head $ ws !! 1 -- "a"