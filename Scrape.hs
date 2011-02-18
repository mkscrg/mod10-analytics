module Main ( main ) where


import Control.DeepSeq ( deepseq )
import Data.Foldable ( foldl' )
import Data.List ( intersperse )
import System ( getArgs )


main :: IO ()
main = do
    argv <- getArgs
    let fname = head argv
    raw <- readFile fname
    let stats = getRawStats raw
    putStrLn fname
    putStrLn $ show $ getCleanStats stats
    putStrLn $ " win histogram: " ++ (show (winHist stats))
    putStrLn $ "loss histogram: " ++ (show (lossHist stats))
    putStrLn ""


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


getRawStats :: String -> RawStats
getRawStats raw = foldl' collect blankRawStats $ filter isEndGame $ lines raw
  where
    collect stats str =
        case read str of
          Win i   -> stats { winN = winN stats + 1
                           , winMin = min i $ winMin stats
                           , winMax = max i $ winMax stats
                           , winSumX = winSumX stats + fromIntegral i
                           , winSumXX = winSumXX stats + sqr i
                           , winHist = let lH' = incHist i $ winHist stats
                                       in lH' `deepseq` lH' }
          Loss i  -> stats { lossN = lossN stats + 1
                           , lossMin = min i $ lossMin stats
                           , lossMax = max i $ lossMax stats
                           , lossSumX = lossSumX stats + fromIntegral i
                           , lossSumXX = lossSumXX stats + sqr i
                           , lossHist = let lH' = incHist i $ lossHist stats
                                        in lH' `deepseq` lH' }
          Timeout -> stats { timeoutN = timeoutN stats + 1 }
    blankRawStats = RawStats { winN = 0
                             , winMin = 1500
                             , winMax = 0
                             , winSumX = 0
                             , winSumXX = 0
                             , winHist = take 2001 $ repeat 0
                             , lossN = 0
                             , lossMin = 1500
                             , lossMax = 0
                             , lossSumX = 0
                             , lossSumXX = 0
                             , lossHist = take 2001 $ repeat 0
                             , timeoutN = 0 }
    isEndGame str = or $ map (== head str) "WLT"
    sqr i = fromIntegral $ i^(2::Int)
    incHist i xs = let (a, b) = splitAt i xs
                   in a ++ (head b + 1) : tail b


nFrac :: Int -> Int -> (Int, Double)
nFrac m n = (m', m'' / fromIntegral n)
  where
    m' = fromIntegral m
    m'' = fromIntegral m

meanSD :: Int -> Integer -> Integer -> (Double, Double)
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
    show sts = concat $ intersperse "\n"
        [ "  games played: " ++ show (count sts)
        , "winning "
        , "      (n,frac): " ++ show (winNFrac sts)
        , "     (min,max): " ++ show (winMinMax sts)
        , "  (mean,stDev): " ++ show (winMeanSD sts)
        , "losing "
        , "      (n,frac): " ++ show (lossNFrac sts)
        , "     (min,max): " ++ show (lossMinMax sts)
        , "  (mean,stDev): " ++ show (lossMeanSD sts)
        , "timeout "
        , "      (n,frac): " ++ show (timeoutNFrac sts) ]


data RawStats = RawStats { winN :: !Int
                         , winMin :: !Int
                         , winMax :: !Int
                         , winSumX :: !Integer
                         , winSumXX :: !Integer
                         , winHist :: ![Int]
                         , lossN :: !Int
                         , lossMin :: !Int
                         , lossMax :: !Int
                         , lossSumX :: !Integer
                         , lossSumXX :: !Integer
                         , lossHist :: ![Int]
                         , timeoutN :: !Int
                         } deriving (Show)


data GameEnds = Win Int
              | Loss Int
              | Timeout
                deriving ( Read
                         , Show )
