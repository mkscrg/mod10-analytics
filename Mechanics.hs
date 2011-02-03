module Mechanics
    (
      GameState(..)
    , play
    , feed
    , advance
    ) where


import Data.List
import Data.Maybe

import Card


play :: GameState -> Maybe GameState
play g@(GameState _ ss i) =
    if isJust canPlay
    then Just (unfeed (fromJust canPlay) g)
    else Nothing
  where
    canPlay = playable (ss !! i)


feed :: GameState -> GameState
feed (GameState d ss i) = GameState (tail d) ss' i
  where
    ss' = (take i ss) ++ [s] ++ (drop (i + 1) ss)
    s = head d : ss !! i


advance :: GameState -> Maybe GameState
advance (GameState d ss i) = if not (and (map null ss))
                               then Just (GameState d ss (adv i))
                               else Nothing
  where
    adv j = let j' = (j + 1) `mod` length ss
            in if null (ss !! j')
                 then adv j'
                 else j'


unfeed :: UFMode -> GameState -> GameState
unfeed UFM0 (GameState d ss i) = GameState (d ++ cs) ss' i
  where
    cs = take 3 s
    ss' = take i ss ++ [drop 3 s] ++ drop (i + 1) ss
    s = ss !! i
unfeed UFM1 (GameState d ss i) = GameState (d ++ cs) ss' i
  where
    cs = take 2 s ++ [last s]
    ss' = take i ss ++ [drop 2 (init s)] ++ drop (i + 1) ss
    s = ss !! i
unfeed UFM2 (GameState d ss i) = GameState (d ++ cs) ss' i
  where
    cs = head s : drop (length s - 2) s
    ss' = take i ss ++ [tail (take (length s - 2) s)]  ++ drop (i + 1) ss
    s = ss !! i


playable :: [Card] -> Maybe UFMode
playable cs = case ufi of
                Just 0  -> Just UFM2
                Just 1  -> Just UFM1
                Just 2  -> Just UFM0
                _       -> Nothing
  where
    ufi = findIndex ((==) 0 . (`mod` 10)) (map (sum . map getVal) (trips cs))


trips :: [a] -> [[a]]
trips cs | l >= 3 = [t2, t1, t0]  -- prefer to take cards from stack bottom
  where
    t0 = take 3 cs
    t1 = take 2 cs ++ [last cs]
    t2 = head cs : drop (l - 2) cs
    l = length cs
trips _           = []


data GameState = GameState [Card] [[Card]] Int

instance Show GameState where
    show (GameState d ss i) = d' ++ ss' ss 0
      where
        d' = "  D: " ++ contents d
        ss' (s:t) j = (if j == i
                            then "->"
                            else "  ") ++ show j ++ ": " ++ contents s ++
            ss' t (j + 1)
        ss' _ _     = ""
        contents s = concat (intersperse " " (map show (reverse s))) ++ "\n"





--    show (GameState d ss i) = concat (replicate (i + 1) "     ") ++
--        "\\/\nD " ++ header (length ss) ++ rows 0 tab
--      where
--        rows j t | j < maxl = row j t ++ rows (j + 1) t
--        rows _ _                              = ""
--        row _ [] = "\n"
--        row k (x:xs) = (if k < length x
--                          then show (x !! k)
--                          else "  ") ++ "   " ++ row k xs
--        header 0 = "\n"
--        header n = "   S" ++ show (8 - n) ++ header (n - 1)
--        tab = map reverse (d : ss)
--        maxl = maximum (map length tab)


data UFMode = UFM0
            | UFM1
            | UFM2
              deriving (Show)
