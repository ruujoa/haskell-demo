import System.IO
import Data.Char

main = do
    contents <- readFile "girlfriend"
    writeFile "girlfriendcaps" (map toUpper contents)