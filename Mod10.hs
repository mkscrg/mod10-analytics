module Main ( main ) where


--import System.Random ( newStdGen )

--import Mechanics
import Params


main :: IO ()
main = do
    params <- getParams
    print params
--    playGame params


--playGame :: IO ()
--playGame = do
--    seed <- newStdGen
--    let g = newGame seed
--    print $ run g
--  where
--    run g@(InPlay {}) = run $ play g
--    run g             = g
