import System.IO

main = do
    handle <- openFile "girlfriend" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
    withFile "girlfriend" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
    contents <- readFile "girlfriend"
    putStr contents