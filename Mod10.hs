import System.Random

import Card
import Mechanics

main :: IO ()
main = do
    putStr "How many games?\n"
    inpStr <- getLine
    let i = read inpStr :: Int
    nGames i


nGames :: Int -> IO ()
nGames 0 = return ()
nGames i = do
    g <- newGame
    _ <- inGame g
    nGames (i - 1)


inGame :: GameState -> IO GameState
inGame g@(GameState d _ _) = do
    print g
    if null d
      then do
          putStr "Loss\n-------\n"
          return g
      else do
--          putStr "Ready?\n"
--          _ <- getLine
          case advance (playAll g) of
            Just g' -> inGame (feed g')
            Nothing -> do
                putStr "Win\n-------\n"
                return g


newGame :: IO GameState
newGame = do
    seed <- newStdGen
    return (feed (feed14 (GameState (shuffle seed deck) (replicate 7 []) 0)))


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
    inc (GameState d ss i) = GameState d ss ((i + 1) `mod` length ss)
