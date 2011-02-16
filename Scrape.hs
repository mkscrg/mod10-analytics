module Main where


import System ( getArgs )


main :: IO ()
main = do
    argv <- getArgs
    print argv
