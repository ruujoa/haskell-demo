import System.IO

main = do
    todoItem <- getLine
    appendFile "todo" (todoItem ++ "\n")