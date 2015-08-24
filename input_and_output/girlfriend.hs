import System.IO

main = do
    handle <- openFile "girlfriend" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle