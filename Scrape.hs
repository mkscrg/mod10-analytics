-- | This module defines an executable called Scrape, which takes as input the
-- output of Mod10 and prints a number of statistics regarding that output.
-- These statistics may then be plotted by FreqPlot.
module Main ( main ) where


import Control.DeepSeq ( deepseq )
import Data.Foldable ( foldl' )
import Data.List ( findIndex, intersperse )
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
    putStrLn $ "    win freq.s: " ++ (show (winFreqs stats))
    putStrLn $ "   loss freq.s: " ++ (show (lossFreqs stats))
    putStrLn ""


-- | Curate RawStats into CleanStats
getCleanStats :: RawStats -> CleanStats
getCleanStats stats =
    CleanStats { count = n
               , winNFrac = nFrac (winN stats) n
               , winMinMax = ( fromIntegral $ winMin stats
                             , fromIntegral $ winMax stats )
               , winMeanSD = meanSD (winN stats) (winSumX stats)
                                    (winSumXX stats)
               , lossNFrac = nFrac (lossN stats) n
               , lossMinMax = ( fromIntegral $ lossMin stats
                              , fromIntegral $ lossMax stats )
               , lossMeanSD = meanSD (lossN stats) (lossSumX stats)
                                    (lossSumXX stats)
               , timeoutNFrac = nFrac (timeoutN stats) n }
  where
    n = winN stats + lossN stats + timeoutN stats


-- | Accumulate RawStats from the given input String. This function uses a
-- strict left fold, DeepSeq, and the strictness annotations on RawStats to
-- ensure no memory leakage while accumulating over lots of input.
getRawStats :: String -> RawStats
getRawStats raw = foldl' collect blankRawStats $ filter isEndGame $ lines raw
  where
    -- Read a GameEnd from an endgame-defining line and update the RawStats
    collect stats str =
        case read str of
          Win i   -> stats { winN = winN stats + 1
                           , winMin = min i $ winMin stats
                           , winMax = max i $ winMax stats
                           , winSumX = winSumX stats + fromIntegral i
                           , winSumXX = winSumXX stats + sqr i
                           , winFreqs = let wFs = incFreqs i $ winFreqs stats
                                        in wFs `deepseq` wFs }
          Loss i  -> stats { lossN = lossN stats + 1
                           , lossMin = min i $ lossMin stats
                           , lossMax = max i $ lossMax stats
                           , lossSumX = lossSumX stats + fromIntegral i
                           , lossSumXX = lossSumXX stats + sqr i
                           , lossFreqs = let lFs = incFreqs i $ lossFreqs stats
                                         in lFs `deepseq` lFs }
          Timeout -> stats { timeoutN = timeoutN stats + 1 }
    sqr i = fromIntegral $ i^(2::Int)
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
meanSD :: Int               -- ^ Number of games being
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
blankRawStats = RawStats { winN = 0
                         , winMin = tOCount
                         , winMax = 0
                         , winSumX = 0
                         , winSumXX = 0
                         , winFreqs = wFs
                         , lossN = 0
                         , lossMin = tOCount
                         , lossMax = 0
                         , lossSumX = 0
                         , lossSumXX = 0
                         , lossFreqs = lFs
                         , timeoutN = 0 }
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
    show sts = concat $ intersperse "\n"
        [ "  games played: " ++ show (count sts)
        , "winning"
        , "      (n,frac): " ++ show (winNFrac sts)
        , "     (min,max): " ++ show (winMinMax sts)
        , "  (mean,stDev): " ++ show (winMeanSD sts)
        , "losing"
        , "      (n,frac): " ++ show (lossNFrac sts)
        , "     (min,max): " ++ show (lossMinMax sts)
        , "  (mean,stDev): " ++ show (lossMeanSD sts)
        , "timeout"
        , "      (n,frac): " ++ show (timeoutNFrac sts) ]


-- | Raw statistics accumulated from the input, with strict record types to
-- prevent memory leakage when accumulating over lots of input.
data RawStats =
      RawStats { winN :: !Int
               -- ^ Number of wins
               , winMin :: !Int
               -- ^ Smallest # of turns in a winning game
               , winMax :: !Int
               -- ^ Greatest # of turns in a winning game
               , winSumX :: !Integer
               -- ^ Sum of # of turns in winning games
               , winSumXX :: !Integer
               -- ^ Sum of squares of # of turns in winning games
               , winFreqs :: ![(Int,Int)]
               -- ^ List of (# of turns, # of wins) pairs
               , lossN :: !Int
               -- ^ Number of losses
               , lossMin :: !Int
               -- ^ Smallest # of turns in a losing game
               , lossMax :: !Int
               -- ^ Greatest # of turns in a losing game
               , lossSumX :: !Integer
               -- ^ Sum of # of turns in losing games
               , lossSumXX :: !Integer
               -- ^ Sum of squares of # of turns in losing games
               , lossFreqs :: ![(Int,Int)]
               -- ^ List of (# of turns, # of losses) pairs
               , timeoutN :: !Int
               -- ^ Number of timeouts
               } deriving (Show)


-- | Allows for the reading of ending GameStates from input.
data GameEnds = Win Int
              | Loss Int
              | Timeout
                deriving ( Read
                         , Show )
