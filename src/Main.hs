import System.Environment
import System.IO
import Day1

dispatch :: [(Int, String -> String)]
dispatch = [
    (1, Day1.solution)
    ]

main = do
    input <- getContents
    (command:_) <- getArgs
    let Just action = lookup (read command) dispatch
    print $ action input ++ "\n"