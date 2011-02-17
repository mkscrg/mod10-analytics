module Main ( main ) where


import System.IO ( hClose
                 , hPrint
                 , hPutStr )
import System.Random ( newStdGen )

import Mechanics
import Params


main :: IO ()
main = do
    params <- getParams
    playGame params
    hClose $ outH params


playGame :: RunParams -> IO ()
playGame RunParams {nGames=n} | n == 0              = return ()
playGame rp@(RunParams {nGames=n, outH=h,verbose=v}) = do
    seed <- newStdGen
    let g = newGame seed
    g' <- run g
    hPrint h g'
    if v
      then hPutStr h "\n\n"
      else return ()
    playGame rp { nGames = n - 1 }
  where
    run g@(InPlay {}) = do if v
                             then do hPrint h g
                                     hPutStr h "\n"
                             else return ()
                           run $ play g
    run g             = return g
