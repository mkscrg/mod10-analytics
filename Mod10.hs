-- | This sourcefile defines an executable called Mod10, which runs a computer
-- simulation of the card game by the same name. By default, Mod10 plays a
-- single game and simply outputs "Win", "Loss", or "Timeout", as well as the
-- number of turns in the game in the former two cases. See "Mechanics" for
-- further details on the game logic. Command-line arguments may be provided
-- for additional functionality, as described in "Params".
module Main ( main ) where


import System.IO ( hClose, hPrint, hPutStr )
import System.Random ( newStdGen )

import Simulation.Mechanics
import Simulation.Params


-- | Play the game by the parameters parsed by 'getParams'.
main :: IO ()
main = do
    params <- getParams
    playGame params
    hClose $ outH params


-- | Generate a new game, play it round-by-round, and print the final
-- GameState. Print the GameState in each round if the verbose parameter is on.
-- Recurse and play another game if the number of games parameter is not 1.
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
    run g@(InPlay {}) = do
        if v
          then do hPrint h g
                  hPutStr h "\n"
          else return ()
        run $ play g
    run g = return g
