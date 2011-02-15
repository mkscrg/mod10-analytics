module Main (main) where


import System.Random (newStdGen)

import Mechanics


main :: IO ()
main = do
    seed <- newStdGen
    print $ newGame seed
