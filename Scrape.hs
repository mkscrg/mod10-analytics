-- | This module defines an executable called Scrape, which takes as input the
-- output of Mod10 and prints a number of statistics regarding that output.
-- These statistics may then be plotted by FreqPlot.
-- TODO: This implementation is somewhat redundant. The RawStats record needn't
-- include anything more than the winFreqs and lossFreqs members. All other
-- information can be computed after accumulating those lists. Fix this!
module Main ( main ) where


import Control.DeepSeq ( deepseq )
import Data.Foldable ( foldl' )
import Data.List ( find, findIndex, intersperse )
import Data.Maybe ( fromJust )
import System ( getArgs )


-- | The first command-line argument is taken as the input filename. Raw
-- statistics data are accumulated over that file and then curated and printed.
-- Frequency data for wins and losses, described below, is also printed.
main :: IO ()
main = do
    argv <- getArgs
    let fname = head argv
    raw <- readFile fname
    let stats = getRawStats raw
    putStrLn fname
    putStrLn $ show $ getCleanStats stats
    putStrLn $ "    win freq.s: " ++ (show $ wins stats)
    putStrLn $ "   loss freq.s: " ++ (show $ losses stats)
    putStrLn ""


-- | Curate RawStats into CleanStats
getCleanStats :: RawStats -> CleanStats
getCleanStats stats =
    CleanStats { count = n
               , winNFrac = nFrac winN n
               , winMinMax = ( fst $ fromJust $ find ((/= 0) . snd) $
                                                     wins stats
                             , fst $ fromJust $ find ((/= 0) . snd) $
                                                     reverse $ wins stats )
               , winMeanSD = meanSD winN (fst winSums) (snd winSums)
               , lossNFrac = nFrac lossN n
               , lossMinMax = ( fst $ fromJust $ find ((/= 0) . snd) $
                                                      losses stats
                              , fst $ fromJust $ find ((/= 0) . snd) $
                                                      reverse $ losses stats )
               , lossMeanSD = meanSD lossN (fst lossSums) (snd lossSums)
               , timeoutNFrac = nFrac (timeouts stats) n }
  where
    n = winN + lossN + timeouts stats
    winN = sum $ map snd (wins stats)
    lossN = sum $ map snd (losses stats)
    winSums = sums $ wins stats
    lossSums = sums $ losses stats
    sums :: [(Int, Int)] -> (Integer, Integer)
    sums freqs = foldr (\(x, x2) (s, s2) ->
                        (x + s, x2 + s2)) (0, 0) $
                       map (\(c, r) ->
                            let (c', r') = (fromIntegral c, fromIntegral r)
                            in (c' * r', c' * c' * r')) freqs


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


-- | Compute the fraction of two numbers, and return with the numerator
nFrac :: Int            -- ^ Numerator
      -> Int            -- ^ Denominator
      -> (Int, Double)  -- ^ Computed (num, num/denom)
nFrac m n = (m, fromIntegral m / fromIntegral n)


-- | Compute the mean and standard deviation from accumulated values.
meanSD :: Int               -- ^ Number of games
       -> Integer           -- ^ Sum of # of turns in games
       -> Integer           -- ^ Sum of squares of # of turns in games
       -> (Double, Double)  -- ^ Computed (mean, standard deviation)
meanSD n sumX sumXX =
    (xBar, ((n' * xBar**2 - 2 * xBar * sumX' + sumXX') / (n' - 1))**0.5)
  where
    xBar = sumX' / n'
    sumXX' = fromIntegral sumXX
    sumX' = fromIntegral sumX
    n' = fromIntegral n


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

-- | Curated statistics, processed from raw statistics
data CleanStats =
    CleanStats { count :: Int
               -- ^ Number of games played
               , winNFrac :: (Int, Double)
               -- ^ Number and fraction of wins
               , winMinMax :: (Int, Int)
               -- ^ Least and greatest # of turns in wins
               , winMeanSD :: (Double, Double)
               -- ^ Mean and standard deviation of # of turns in wins
               , lossNFrac :: (Int, Double)
               -- ^ Number and fraction of wins
               , lossMinMax :: (Int, Int)
               -- ^ Least and greatest # of turns in wins
               , lossMeanSD :: (Double, Double)
               -- ^ Mean and standard deviation of # of turns in wins
               , timeoutNFrac :: (Int, Double)
               -- ^ Number and fraction of wins
               }

-- | Produce a String representation of the curated statistics
instance Show CleanStats where
    show stats = concat $ intersperse "\n"
        [ "  games played: " ++ show (count stats)
        , "winning"
        , "      (n,frac): " ++ show (winNFrac stats)
        , "     (min,max): " ++ show (winMinMax stats)
        , "  (mean,stDev): " ++ show (winMeanSD stats)
        , "losing"
        , "      (n,frac): " ++ show (lossNFrac stats)
        , "     (min,max): " ++ show (lossMinMax stats)
        , "  (mean,stDev): " ++ show (lossMeanSD stats)
        , "timeout"
        , "      (n,frac): " ++ show (timeoutNFrac stats) ]


-- | Raw statistics accumulated from the input, with strict members to prevent
-- memory leakage when accumulating over lots of input.
data RawStats =
      RawStats { wins :: ![(Int,Int)]
               -- ^ List of (# of turns, # of wins) pairs
               , losses :: ![(Int,Int)]
               -- ^ List of (# of turns, # of losses) pairs
               , timeouts :: Int
               -- ^ Number of timeouts
               } deriving (Show)


-- | Allows for the reading of ending GameStates from input.
data GameEnds = Win Int
              | Loss Int
              | Timeout
                deriving ( Read
                         , Show )
