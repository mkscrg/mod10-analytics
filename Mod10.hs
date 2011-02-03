module Main ( main ) where

import System.Random ( newStdGen )
import System ( getArgs )

import Card
import Mechanics


main :: IO ()
main = do
    args <- getArgs
    nGames (read (head args) :: Int)


nGames :: Int -> IO ()
nGames 0 = do
    return ()
nGames i = do
    g <- newGame
    _ <- inGame g
    nGames (i - 1)


inGame :: GameState -> IO GameState
inGame g = do
    let g' = g { ctr = ctr g + 1 }
    print g'
    if null (deck g')
      then do putStr ("% L " ++ show (ctr g') ++ " %\n\n\n")
              return g'
      else if ctr g > 1500
             then do putStr ("% T " ++ show (ctr g') ++ " %\n\n\n")
                     return g'
             else do
--                 putStr "Ready?\n"
--                 _ <- getLine
                 case advance (playAll g') of
                   Just g'' -> inGame (feed g'')
                   Nothing -> do putStr ("% W " ++ show (ctr g') ++ " %\n\n\n")
                                 return g'


newGame :: IO GameState
newGame = do
    seed <- newStdGen
    return (feed (feed14 (GameState { deck = shuffle seed newdeck
                                    , stacks = replicate 7 []
                                    , csi = 0
                                    , ctr = 0 })))


playAll :: GameState -> GameState
playAll g = case play g of
              Just g' -> playAll g'
              Nothing -> g


feed14 :: GameState -> GameState
feed14 g = feed' 14 g
  where
    feed' :: Int -> GameState -> GameState
    feed' 0 g'                  = g'
    feed' j g' = feed' (j - 1) (inc (feed g'))
    inc g' = g' { csi = (csi g' + 1) `mod` length (stacks g') }
