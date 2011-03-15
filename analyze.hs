-- | This module defines an executable called @analyze@, which takes as input
-- the output of @simulate@ and prints a number of statistics about the
-- simulation. See 'Simulation.Scrape' for the details of the data
-- accumulation. Plots of wins/losses versus number of rounds may be generated
-- via the @-p@ flag. See 'Simulation.Plots' for details on this, and
-- 'Simulation.Params' for the full set of command-line options.
module Main ( main ) where


import Data.List ( find, foldl', intersperse )
import System.Exit ( ExitCode(..) )
import System.IO ( hClose, hGetContents, hPutStrLn )

import Analysis.Params
import Analysis.Plots
import Analysis.Scrape


-- | Collect data and print statistics, and possibly make plots, according to
-- the options parsed by 'Analysis.Params.getParams'.
main :: IO ()
main = do
    params <- getParams
    inStr <- hGetContents $ inH params  -- Performs a lazy read of the input.
    let rStats = getRawStats inStr
    let cStats = getCleanStats rStats
    hPutStrLn (outH params) $ show cStats
    _ <- if plots params
           then makePlot rStats $ count cStats
           else return ExitSuccess
    hClose $ inH params
    hClose $ outH params


-- | Curate an 'Analysis.Scrape.RawStats' record into a 'CleanStats' record.
getCleanStats :: RawStats -> CleanStats
getCleanStats stats =
    CleanStats { count = n
               , winNFrac = nFrac winN lossN
               , winMinMax = ( findFirst $ wins stats
                             , findFirst $ reverse $ wins stats )
               , winMeanSD = meanSD winN (fst winSums) (snd winSums)
               , lossNFrac = nFrac lossN winN
               , lossMinMax = ( findFirst $ losses stats
                              , findFirst $ reverse $ losses stats )
               , lossMeanSD = meanSD lossN (fst lossSums) (snd lossSums)
               , timeoutNFrac = nFrac (timeouts stats) (winN + lossN) }
  where
    -- The total number of games played
    n = winN + lossN + timeouts stats
    -- Get number of winning games by summing the number of games for each
    -- possible number of rounds.
    winN = foldl' (+) 0 $ map snd (wins stats)
    lossN = foldl' (+) 0 $ map snd (losses stats)
    -- Accumulate the sum of the numbers of rounds, and the sum of the squares
    -- of the numbers of rounds, in a tuple, for wins and losses.
    winSums = sums $ wins stats
    lossSums = sums $ losses stats
    -- Find the first tuple in the list with a non-zero second element.
    findFirst freqs = case find ((/= 0) . snd) freqs of
                        Just (x, _) -> x
                        Nothing     -> -1
    -- We can't use the builtin 'sum', because it uses non-strict foldl.
    sums :: [(Int, Int)] -> (Integer, Integer)
    sums freqs = foldr (\(x, x2) (s, s2) ->
                        (x + s, x2 + s2)) (0, 0) $
                       map (\(c, r) ->
                            let (c', r') = (fromIntegral c, fromIntegral r)
                            in (c' * r', c' * c' * r')) freqs


-- | Compute the fraction of two numbers, and return with the numerator
nFrac :: Int            -- ^ numer
      -> Int            -- ^ denom' = denom - numer
      -> (Int, Double)  -- ^ Computed (numer, numer / denom' + numer)
nFrac m n = (m, m' / (m' + fromIntegral n))
  where
    m' = fromIntegral m


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


-- | Curated statistics, generally processed from an 'Analysis.Scrape.RawStats'
-- record.
data CleanStats =
    CleanStats { count :: Int
               -- ^ Number of games played
               , winNFrac :: (Int, Double)
               -- ^ Number and fraction of wins in non-timeouts
               , winMinMax :: (Int, Int)
               -- ^ Least and greatest # of turns in wins
               , winMeanSD :: (Double, Double)
               -- ^ Mean and standard deviation of # of turns in wins
               , lossNFrac :: (Int, Double)
               -- ^ Number and fraction of losses in non-timeouts
               , lossMinMax :: (Int, Int)
               -- ^ Least and greatest # of turns in losses
               , lossMeanSD :: (Double, Double)
               -- ^ Mean and standard deviation of # of turns in losses
               , timeoutNFrac :: (Int, Double)
               -- ^ Number and fraction of timeouts in total
               }

instance Show CleanStats where
    show stats = concat $ intersperse "\n"
        [ "# games played: " ++ show (count stats)
        , "winning ..."
        , "  (# games, frac. of non-timeouts): " ++ show (winNFrac stats)
        , "      (min # rounds, max # rounds): " ++ show (winMinMax stats)
        , "   (mean # rounds, stDev # rounds): " ++ show (winMeanSD stats)
        , "losing ..."
        , "  (# games, frac. of non-timeouts): " ++ show (lossNFrac stats)
        , "      (min # rounds, max # rounds): " ++ show (lossMinMax stats)
        , "   (mean # rounds, stDev # rounds): " ++ show (lossMeanSD stats)
        , "timeout ..."
        , "         (# games, frac. of total): " ++ show (timeoutNFrac stats) ]
