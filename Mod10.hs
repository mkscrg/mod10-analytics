module Main ( main ) where


import System.Random ( newStdGen )

import Mechanics
import Pregame


main :: IO ()
main = do
    seed <- newStdGen
    print $ newGame seed
