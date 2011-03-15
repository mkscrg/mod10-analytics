-- | This module defines an executable called @simulate@, which runs a
-- simulation of the card game called Mod10. By default, @simulate@ plays a
-- single game and simply outputs "Win", "Loss", or "Timeout", as well as the
-- number of turns in the game for a win or loss. See 'Simulation.Mechanics'
-- for further details on the game logic. Command-line arguments may be
-- provided for additional functionality, as described in 'Simulation.Params'.
module Main ( main ) where


import System.IO ( hClose, hPrint, hPutStr )
import System.Random ( newStdGen )

import Simulation.Mechanics
import Simulation.Params


-- | Play the game according the options parsed by
-- 'Simulation.Params.getParams'.
main :: IO ()
main = do
    params <- getParams
    playGame params
    hClose $ outH params


-- | Generate a new game, play it round-by-round, and print the final
-- GameState. Print the GameState in each round if the verbose parameter is on.
-- Recurse and play another game if the number of games parameter is not 1.
playGame :: RunParams -> IO ()
playGame RunParams {nGames=n} | n == 0 = return ()
playGame rp@(RunParams {filt=f, nGames=n, outH=h, verbose=v}) = do
    seed <- newStdGen
    let g = newGame seed
    g' <- if f
            then runFilter g
            else run g
    hPrint h g'
    if v
      then hPutStr h "\n\n"
      else return ()
    playGame rp { nGames = n - 1 }
  where
    -- Play a GameState to completion
    run g@(InPlay {}) = do
        if v
          then do hPrint h g
                  hPutStr h "\n"
          else return ()
        run $ play g
    run g = return g
    -- Play a GameState until the end of turn 7, then time out if at least
    -- @threshold@ stacks haven't been eliminated. (Activated via the
    -- -f/--filter flag.)
    runFilter g@(InPlay {}) = do
        if v
          then do hPrint h g
                  hPutStr h "\n"
          else return ()
        case ctr g of
          c | c < 7 -> runFilter $ play g
          _ -> if length (filter (not . null) $
                                 stacks g) <= length (stacks g) - threshold
                 then run $ play g
                 else return Timeout
    runFilter g = run g
    threshold = 1
