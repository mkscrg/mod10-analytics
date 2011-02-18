module Mechanics ( play
                 , newGame
                 , GameState(..)
                 ) where


import Data.List ( elemIndex
                 , intersperse )
import System.Random ( RandomGen )

import Card


play :: GameState -> GameState
play InPlay {deck=d, ctr=n}    | null d            = Loss n
play InPlay {stacks=ss, ctr=n} | and $ map null ss = Win n
play InPlay {ctr=n}            | n >= 2000          = Timeout
play g@(InPlay {})                                 = feedNext $ pickUp g
play g                                             = g


newGame :: (RandomGen g) => g -> GameState
newGame rgen = let (s, d) = splitAt 15 $ shuffle rgen newDeck
               in InPlay { deck = d
                         , stacks = feedAll s $ replicate 7 []
                         , csi = 0
                         , ctr = 0 }


pickUp :: GameState -> GameState
pickUp g@(InPlay {deck=d, stacks=ss, csi=i}) =
    case findTriplet s of
      Just Top    -> let (s', d') = moveTriplet Top s d
                     in pickUp $ g { deck = d', stacks = newCurrStack s' }
      Just Middle -> let (s', d') = moveTriplet Middle s d
                     in pickUp $ g { deck = d', stacks = newCurrStack s' }
      Just Bottom -> let (s', d') = moveTriplet Bottom s d
                     in pickUp $ g { deck = d', stacks = newCurrStack s' }
      Nothing     -> g
  where
    newCurrStack s' = let (a, b) = splitAt i ss
                      in a ++ s' : tail b
    s = ss !! i
pickUp g = g


feedNext :: GameState -> GameState
feedNext g@(InPlay {deck=d, stacks=ss, csi=i}) =
    case nextI of
      Nothing -> g { ctr = ctr g + 1 }
      Just i' -> g { deck = tail d
                   , stacks = let (a, b) = splitAt i' ss
                              in a ++ (head d : head b) : tail b
                   , csi = i'
                   , ctr = ctr g + 1 }
  where
    nextI = let mayI = elemIndex True $ drop (i + 1) $
                       concat $ replicate 2 $ map (not . null) ss
            in case mayI of
                 Nothing -> Nothing
                 Just i' -> Just $ (i + i' + 1) `mod` length ss
feedNext g = g


moveTriplet :: Triplet -> [a] -> [a] -> ([a], [a])
moveTriplet Top    src dst = let (a, b) = splitAt (length src - 2) src
                             in (tail a, dst ++ head a : b)
moveTriplet Middle src dst = let (a, b) = splitAt 2 src
                             in (init b, dst ++ a ++ [last b])
moveTriplet Bottom src dst = let (a, b) = splitAt 3 src
                             in (b, dst ++ a)


findTriplet :: (HasValue a) => [a] -> Maybe Triplet
findTriplet cs | len >= 3 =
    if valTrip $ head cs : drop (len - 2) cs
      then Just Top
      else if valTrip $ take 2 cs ++ [last cs]
             then Just Middle
             else if valTrip $ take 3 cs
                    then Just Bottom
                    else Nothing
  where
    valTrip = (== 0) . (`mod` 10) . sum . map getVal
    len = length cs
findTriplet _ = Nothing


feedAll :: [a] -> [[a]] -> [[a]]
feedAll src dst = feeder padSrc dst
  where
    feeder [] d = d
    feeder s  d = let (a, b) = splitAt lend s
                  in feeder b $ zipWith mayCat d a
    mayCat xs (Just x) = x : xs
    mayCat xs Nothing  = xs
    padSrc = map Just src ++ replicate (lend - length src `mod` lend) Nothing
    lend = length dst


data GameState = Win Int
               | Loss Int
               | Timeout
               | InPlay { deck :: [Card]
                        , stacks :: [[Card]]
                        , csi :: Int
                        , ctr :: Int }

instance Show GameState where
    show (Win i)  = "Win " ++ show i
    show (Loss i) = "Loss " ++ show i
    show Timeout  = "Timeout"
    show InPlay {deck=d, stacks=ss, csi=i, ctr=n} =
        concat $ intersperse "\n" $
            [" n: " ++ show n, " d: " ++ stackToStr d] ++
            zipWith (++) (map label [0..(length ss - 1)])
                         (map stackToStr ss)
      where
        stackToStr s = concat $ intersperse " " $ map show (reverse s)
        label j = (if j == i
                     then '*'
                     else ' ') : show j ++ ": "


data Triplet = Top
             | Middle
             | Bottom
               deriving (Show)
