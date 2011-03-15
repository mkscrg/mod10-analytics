-- | The 'Simulation.Mechanics' module defines the game logic of Mod10, from
-- detection of valid triplets to playing whole turns, for the @simulate@
-- executable.
module Simulation.Mechanics ( play, newGame
                            , GameState(..)
                            ) where


import Data.List ( elemIndex, intersperse )
import System.Random ( RandomGen )

import Simulation.Card


-- | Play a single turn of the game, first checking for 'Win'/'Loss'/'Timeout'.
play :: GameState -> GameState
play InPlay {deck=d, ctr=n}    | null d            = Loss n
play InPlay {stacks=ss, ctr=n} | and $ map null ss = Win n
play InPlay {ctr=n}            | n >= 2000         = Timeout
play g@(InPlay {})                                 = feedNext $ pickUp g
play g                                             = g


-- | Produce a ready-to-play 'GameState' from a shuffled deck.
newGame :: (RandomGen r) => r -> GameState
newGame rgen = let (s, d) = splitAt (nStacks * 2 + 1) $ shuffle rgen newDeck
               in InPlay { deck = d
                         , stacks = feedAll s $ replicate nStacks []
                         , csi = 0
                         , ctr = 0 }
  where
    nStacks = 7


-- | Pick up all valid triplets in the current stack.
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


-- | Deal the top of the deck to the top of the current stack, and increment
-- the turn counter. If no stacks are left (a winning game), just increment the
-- turn counter.
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
    -- Find the first nonempty stack after the current stack. Use Maybe to
    -- alert cases in which there are no nonempty stacks.
    nextI = let mayI = elemIndex True $ drop (i + 1) $
                       concat $ replicate 2 $ map (not . null) ss
            in case mayI of
                 Nothing -> Nothing
                 Just i' -> Just $ (i + i' + 1) `mod` length ss
feedNext g = g


-- | Move cards, designated by a 'Triplet', from the src list to the end of the
-- dst list.
moveTriplet :: Triplet     -- ^ Designates the cards to be taken from src
            -> [a]         -- ^ src list (usually a stack)
            -> [a]         -- ^ dst stack (usually the deck)
            -> ([a], [a])  -- ^ Modified (source, destination)
moveTriplet Top    src dst = let (a, b) = splitAt (length src - 2) src
                             in (tail a, dst ++ head a : b)
moveTriplet Middle src dst = let (a, b) = splitAt 2 src
                             in (init b, dst ++ a ++ [last b])
moveTriplet Bottom src dst = let (a, b) = splitAt 3 src
                             in (b, dst ++ a)


-- | Check a list of 'Card's for a valid triplet. In the case of multiple valid
-- triplets, order of precedence is 'Top', 'Middle', then 'Bottom'. (This is
-- probably not the optimal strategy, but it's a relatively rare choice.)
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


-- | "Deal" the elements of the src list into the dst list of lists, by
-- zipWith'ing the dst list with dst-length pieces of src, by list
-- construction.
feedAll :: [a]    -- ^ src list
        -> [[a]]  -- ^ dst list of lists
        -> [[a]]  -- ^ Modified dst
feedAll src dst = feeder padSrc dst
  where
    feeder [] d = d
    feeder s  d = let (a, b) = splitAt lend s
                  in feeder b $ zipWith mayCat d a
    mayCat xs (Just x) = x : xs
    mayCat xs Nothing  = xs
    padSrc = map Just src ++ replicate (lend - length src `mod` lend) Nothing
    lend = length dst


data GameState =
    Win Int   -- ^ A won game, with the number of turns played
  | Loss Int  -- ^ A lost game, with the number of turns played
  | Timeout   -- ^ A game that's exceeded the time-out limit
  | InPlay { deck :: [Card]      -- ^ The deck from which cards are dealt
           , stacks :: [[Card]]  -- ^ The stacks to which cards are dealt
           , csi :: Int          -- ^ The (0-based) index of the current stack
           , ctr :: Int }        -- ^ The number of turns played

-- | Produce a human-readable String representation of all aspects of a
-- 'GameState'.
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
        -- Print stacks (and deck) in reverse, so that the "top" card is to the
        -- right of the screen.
        stackToStr s = concat $ intersperse " " $ map show (reverse s)
        -- Mark the current stack with *.
        label j = (if j == i
                     then '*'
                     else ' ') : show j ++ ": "


data Triplet = Top              -- ^ The top 2 cards and the bottom card
             | Middle           -- ^ The top card and the bottom 2 cards
             | Bottom           -- ^ The bottom 3 cards
               deriving (Show)
