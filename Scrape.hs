module Main where


import Data.Foldable ( foldl' )
import Data.List ( intersperse )
import System ( getArgs )


main :: IO ()
main = do
    argv <- getArgs
    raw <- readFile $ head argv
    let stats = getRawStats raw
    print $ getCleanStats stats


getCleanStats :: RawStats -> CleanStats
getCleanStats stats =
    CleanStats { count = n
               , winNFrac = nFrac (winN stats) n
               , winMinMax = (winMin stats, winMax stats)
               , winMeanSD = meanSD (winN stats) (winSumX stats)
                                    (winSumXX stats)
               , lossNFrac = nFrac (lossN stats) n
               , lossMinMax = (lossMin stats, lossMax stats)
               , lossMeanSD = meanSD (lossN stats) (lossSumX stats)
                                    (lossSumXX stats)
               , timeoutNFrac = nFrac (timeoutN stats) n }
  where
    n = winN stats + lossN stats + timeoutN stats


getRawStats :: String -> RawStats
getRawStats raw = foldl' collect blankRawStats $ filter isEndGame $ lines raw
  where
    collect stats str =
        case read str of
          Win i   ->  stats { winN = winN stats + 1
                            , winMin = min i $ winMin stats
                            , winMax = max i $ winMax stats
                            , winSumX = winSumX stats + i
                            , winSumXX = winSumXX stats + sqr i }
          Loss i  ->  stats { lossN = lossN stats + 1
                            , lossMin = min i $ lossMin stats
                            , lossMax = max i $ lossMax stats
                            , lossSumX = lossSumX stats + i
                            , lossSumXX = lossSumXX stats + sqr i }
          Timeout ->  stats { timeoutN = timeoutN stats + 1 }
    blankRawStats = RawStats { winN = 0
                             , winMin = 1500
                             , winMax = 0
                             , winSumX = 0
                             , winSumXX = 0
                             , lossN = 0
                             , lossMin = 1500
                             , lossMax = 0
                             , lossSumX = 0
                             , lossSumXX = 0
                             , timeoutN = 0 }
    isEndGame str = or $ map (== head str) "WLT"
    sqr i = fromIntegral $ i^(2::Int)


nFrac :: Int -> Int -> (Int, Double)
nFrac m n = (m, m' / fromIntegral n)
  where
    m' = fromIntegral m

meanSD :: Int -> Int -> Integer -> (Double, Double)
meanSD n sumX sumXX =
    (xBar, ((n' * xBar**2 - 2 * xBar * sumX' + sumXX') / (n' - 1))**0.5)
  where
    xBar = sumX' / n'
    sumXX' = fromIntegral sumXX
    sumX' = fromIntegral sumX
    n' = fromIntegral n


data CleanStats = CleanStats { count :: Int
                             , winNFrac :: (Int, Double)
                             , winMinMax :: (Int, Int)
                             , winMeanSD :: (Double, Double)
                             , lossNFrac :: (Int, Double)
                             , lossMinMax :: (Int, Int)
                             , lossMeanSD :: (Double, Double)
                             , timeoutNFrac :: (Int, Double) }

instance Show CleanStats where
    show sts = (concat $ intersperse "\n"
        [ "# games played: " ++ show (count sts)
        , "winning:"
        , "      (n,frac): " ++ show (winNFrac sts)
        , "     (min,max): " ++ show (winMinMax sts)
        , "  (mean,stDev): " ++ show (winMeanSD sts)
        , "losing:"
        , "      (n,frac): " ++ show (lossNFrac sts)
        , "     (min,max): " ++ show (lossMinMax sts)
        , "  (mean,stDev): " ++ show (lossMeanSD sts)
        , "timeout:"
        , "      (n,frac): " ++ show (timeoutNFrac sts) ])
        ++ "\n"


data RawStats = RawStats { winN :: !Int
                         , winMin :: !Int
                         , winMax :: !Int
                         , winSumX :: !Int
                         , winSumXX :: !Integer
                         , lossN :: !Int
                         , lossMin :: !Int
                         , lossMax :: !Int
                         , lossSumX :: !Int
                         , lossSumXX :: !Integer
                         , timeoutN :: !Int
                         } deriving (Show)


data GameEnds = Win Int
              | Loss Int
              | Timeout
                deriving ( Read
                         , Show )
