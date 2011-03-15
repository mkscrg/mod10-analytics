-- WRONG WRONG WRONG WRONG WRONG WRONG WRONG WRONG WRONG WRONG
-- | This module defines an executable called Scrape, which takes as input the
-- output of Mod10 and prints a number of statistics regarding that output.
-- These statistics may then be plotted by FreqPlot.
module Main ( main ) where


import Data.List ( find, foldl', intersperse )
import System.Exit ( ExitCode(..) )
import System.IO ( hClose, hGetContents, hPutStrLn )

import Analysis.Params
import Analysis.Plots
import Analysis.Scrape


-- | The first command-line argument is taken as the input filename. Raw
-- statistics data are accumulated over that file and then curated and printed.
-- Frequency data for wins and losses, described below, is also printed.
main :: IO ()
main = do
    params <- getParams
    inStr <- hGetContents $ inH params
    let rStats = getRawStats inStr
    let cStats = getCleanStats rStats
    hPutStrLn (outH params) $ show cStats
    _ <- if plots params
           then makePlot rStats $ count cStats
           else return ExitSuccess
    hClose $ inH params
    hClose $ outH params


-- | Curate RawStats into CleanStats
getCleanStats :: RawStats -> CleanStats
getCleanStats stats =
    CleanStats { count = n
               , winNFrac = nFrac winN n
               , winMinMax = ( findFirst $ wins stats
                             , findFirst $ reverse $ wins stats )
               , winMeanSD = meanSD winN (fst winSums) (snd winSums)
               , lossNFrac = nFrac lossN n
               , lossMinMax = ( findFirst $ losses stats
                              , findFirst $ reverse $ losses stats )
               , lossMeanSD = meanSD lossN (fst lossSums) (snd lossSums)
               , timeoutNFrac = nFrac (timeouts stats) n }
  where
    n = winN + lossN + timeouts stats
    winN = foldl' (+) 0 $ map snd (wins stats)
    lossN = foldl' (+) 0 $ map snd (losses stats)
    winSums = sums $ wins stats
    lossSums = sums $ losses stats
    findFirst freqs = case find ((/= 0) . snd) freqs of
                        Just (x, _) -> x
                        Nothing     -> -1
    sums :: [(Int, Int)] -> (Integer, Integer)
    sums freqs = foldr (\(x, x2) (s, s2) ->
                        (x + s, x2 + s2)) (0, 0) $
                       map (\(c, r) ->
                            let (c', r') = (fromIntegral c, fromIntegral r)
                            in (c' * r', c' * c' * r')) freqs


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
        [ "games played: " ++ show (count stats)
        , "winning ..."
        , "      (n,frac): " ++ show (winNFrac stats)
        , "     (min,max): " ++ show (winMinMax stats)
        , "  (mean,stDev): " ++ show (winMeanSD stats)
        , "losing ..."
        , "      (n,frac): " ++ show (lossNFrac stats)
        , "     (min,max): " ++ show (lossMinMax stats)
        , "  (mean,stDev): " ++ show (lossMeanSD stats)
        , "timeout ..."
        , "      (n,frac): " ++ show (timeoutNFrac stats) ]
