module Mechanics ( GameState(..)
                 , play
                 , feed
                 , advance
                 ) where


import Data.List
import Data.Maybe

import Card


play :: GameState -> Maybe GameState
play g = if isJust csCanPlay
           then Just (unfeed (fromJust csCanPlay) g)
           else Nothing
  where
    csCanPlay = playable ((stacks g) !! (csi g))


feed :: GameState -> GameState
feed g = g { deck = tail (deck g), stacks = ss }
  where
    ss = take (csi g) (stacks g)
      ++ [cs]
      ++ drop (csi g + 1) (stacks g)
    cs = head (deck g) : (stacks g) !! (csi g)


advance :: GameState -> Maybe GameState
advance g = if not (and (map null (stacks g)))
              then Just g { csi = adv (csi g) }
              else Nothing
  where
    adv j = let j' = (j + 1) `mod` length (stacks g)
            in if null (stacks g !! j')
                 then adv j'
                 else j'


unfeed :: UFMode -> GameState -> GameState
unfeed ufm g = g { deck = d, stacks = ss }
  where
    d = deck g
     ++ case ufm of
          UFM0 -> take 3 cs
          UFM1 -> take 2 cs ++ [last cs]
          UFM2 -> head cs : drop (length cs - 2) cs
    ss = take (csi g) (stacks g)
      ++ case ufm of
           UFM0 -> [drop 3 cs]
           UFM1 -> [drop 2 (init cs)]
           UFM2 -> [tail (take (length cs - 2) cs)]
      ++ drop (csi g + 1) (stacks g)
    cs = (stacks g !! csi g)


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


data GameState = GameState { deck :: [Card]
                           , stacks :: [[Card]]
                           , csi :: Int
                           , ctr :: Int
                           }

instance Show GameState where
    show g = cLine ++ dLine ++ sLines
      where
        cLine = "  i: " ++ show (ctr g) ++ "\n"
        dLine = "  D: " ++ stackStr (deck g) ++ "\n"
        sLines = concat $ intersperse "\n" $
            zipWith (\i s -> (if i == csi g
                                then "->"
                                else "  ") ++ show i ++ ": " ++ s)
                    [0..(length (stacks g) - 1)]
                    (map stackStr (stacks g))
        stackStr p = concat (intersperse " " (map show (reverse p)))


data UFMode = UFM0
            | UFM1
            | UFM2
              deriving (Show)
