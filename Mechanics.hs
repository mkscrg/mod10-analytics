module Mechanics where


import System.Random (RandomGen, newStdGen)

import Card


main :: IO ()
main = do
    seed <- newStdGen
    let g = newGame seed
    print $ stacks g !! 0
    let g' = pickUp g
    print $ stacks g' !! 0
    return ()


play :: GameState -> GameState
play InPlay {deck=d}    | null d            = Loss
play InPlay {stacks=ss} | and $ map null ss = Win
play InPlay {ctr=n}     | n > 1500          = Timeout
--play InPlay {} = 
play g = g


newGame :: (RandomGen g) => g -> GameState
newGame rgen = let (s, d) = splitAt 43 $ shuffle rgen newDeck
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


moveTriplet :: Triplet -> [Card] -> [Card] -> ([Card], [Card])
moveTriplet Top    src dst = let (a, b) = splitAt (length src - 2) src
                             in (tail a, dst ++ head a : b)
moveTriplet Middle src dst = let (a, b) = splitAt 2 src
                             in (init b, dst ++ a ++ [last b])
moveTriplet Bottom src dst = let (a, b) = splitAt 3 src
                             in (b, dst ++ a)


findTriplet :: [Card] -> Maybe Triplet
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
    mayCat xs (Just x) = xs ++ [x]
    mayCat xs Nothing  = xs
    padSrc = map Just src ++ replicate (lend - length src `mod` lend) Nothing
    lend = length dst


data GameState = Win
               | Loss
               | Timeout
               | InPlay { deck :: [Card]
                        , stacks :: [[Card]]
                        , csi :: Int
                        , ctr :: Int }
                 deriving (Show)


data Triplet = Top
             | Middle
             | Bottom
               deriving (Show)
