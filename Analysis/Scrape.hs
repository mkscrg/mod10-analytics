module Analysis.Scrape ( getRawStats
                       , RawStats(..) ) where


import Control.DeepSeq ( deepseq )
import Data.Foldable ( foldl' )
import Data.List ( findIndex )


-- | Accumulate RawStats from the given input String. This function uses a
-- strict left fold, DeepSeq, and the strictness annotations on RawStats to
-- ensure no memory leakage while accumulating over lots of input.
getRawStats :: String -> RawStats
getRawStats raw = foldl' collect blankRawStats $ filter isEndGame $ lines raw
  where
    -- Read a GameEnd from an endgame-defining line and update the RawStats
    collect stats str =
        case read str of
          Win i   -> stats { wins = let wFs = incFreqs i $ wins stats
                                    in wFs `deepseq` wFs }
          Loss i  -> stats { losses = let lFs = incFreqs i $ losses stats
                                      in lFs `deepseq` lFs }
          Timeout -> stats { timeouts = timeouts stats + 1 }
    -- Determine whether a line of input defines an endgame
    isEndGame str | not $ null str = or $ map (== head str) "WLT"
    isEndGame _                    = False
    -- Increment an entry in a frequency list, given the # of turns
    incFreqs i xs = let mi = findIndex (\(i', _) -> i' == i) xs
                    in case mi of
                         Just hi -> let (a, b) = splitAt hi xs
                                    in a ++
                                       (\(i', c) -> (i', c + 1)) (head b) :
                                       tail b
                         Nothing -> xs


-- | An initialized RawStats, as a base for accumulation from input.
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
-- memory leakage when accumulating over lots of input.
data RawStats =
      RawStats { wins :: ![(Int,Int)]
               -- ^ List of (# of turns, # of wins) pairs
               , losses :: ![(Int,Int)]
               -- ^ List of (# of turns, # of losses) pairs
               , timeouts :: !Int
               -- ^ Number of timeouts
               } deriving (Show)


-- | Allows for the reading of ending GameStates from input.
data GameEnds = Win Int
              | Loss Int
              | Timeout
                deriving ( Read
                         , Show )
