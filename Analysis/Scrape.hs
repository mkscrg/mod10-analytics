-- | The 'Analysis.Scrape' module provides the basic data accumulation
-- functionality of the @analyze@ executable, allowing it to traverse the input
-- with a constant-size memory footprint.
module Analysis.Scrape ( getRawStats
                       , RawStats(..) ) where


import Control.DeepSeq ( deepseq )
import Data.Foldable ( foldl' )
import Data.List ( findIndex )


-- | Accumulate a 'RawStats' record from the given input string. This function
-- uses a strict left fold, 'Control.DeepSeq', and strictness annotations on
-- 'RawStats' to ensure a constant-size memory footprint while accumulating
-- over large input.
getRawStats :: String -> RawStats
getRawStats raw = foldl' collect blankRawStats $ filter isEndGame $ lines raw
  where
    -- Read a GameEnd from an endgame-defining line and update the RawStats
    collect stats str =
        case read str of
          Win i   -> stats { wins = let wFs = incFreqs (wins stats) i
                                    in wFs `deepseq` wFs }
          Loss i  -> stats { losses = let lFs = incFreqs (losses stats) i
                                      in lFs `deepseq` lFs }
          Timeout -> stats { timeouts = timeouts stats + 1 }
    -- Determine whether a line of input defines an endgame
    isEndGame str | not $ null str = or $ map (== head str) "WLT"
    isEndGame _                    = False
    -- Increment an entry in a frequency list, given the # of turns
    incFreqs xs i = let mi = findIndex (\(i', _) -> i' == i) xs
                    in case mi of
                         Just hi -> let (a, b) = splitAt hi xs
                                    in a ++
                                       (\(i', c) -> (i', c + 1)) (head b) :
                                       tail b
                         Nothing -> xs


-- | An initialized 'RawStats' record, as a base for accumulation from input.
blankRawStats :: RawStats
blankRawStats = RawStats { wins = wFs
                         , losses = lFs
                         , timeouts = 0 }
  where
    wFs = zip [7,10..tOCount] zeros
    lFs = zip [37,40..tOCount] zeros
    zeros = repeat 0
    tOCount = 2000


-- | Raw statistics accumulated from the input, with strict members to prevent
-- memory leakage when accumulating over large input.
data RawStats =
      RawStats { wins :: ![(Int,Int)]
               -- ^ List of (# of turns, # of wins) pairs
               , losses :: ![(Int,Int)]
               -- ^ List of (# of turns, # of losses) pairs
               , timeouts :: !Int
               -- ^ Number of timeouts
               } deriving (Show)


-- | Allows for simple reading of end-game states from input.
data GameEnds = Win Int
              | Loss Int
              | Timeout
                deriving ( Read
                         , Show )
