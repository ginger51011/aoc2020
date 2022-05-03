import qualified Data.Maybe
import qualified Day1
import qualified Day2
import System.Environment
import System.IO

solve :: ([String] -> Maybe Int) -> [String] -> String
solve _ [] = "No input provided"
solve solver input =
  show $ Data.Maybe.fromMaybe (-1) (solver input)

dispatch :: [(Int, [[String] -> String])]
dispatch =
  [ ( 1,
      [ solve $ Day1.solver1 . map read, -- We want to read these directly to ints
        solve $ Day1.solver2 . map read
      ]
    ),
    (2, [solve Day2.solver1, solve Day2.solver2])
  ]

main = do
  input <- getContents
  (day : part) <- getArgs
  let Just solutionList = lookup (read day) dispatch
      action = solutionList !! (read (head part) - 1) -- Get the part of that day
  print $ action (lines input)