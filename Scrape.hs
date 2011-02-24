module Main ( main ) where


import Control.DeepSeq ( deepseq )
import Data.Foldable ( foldl' )
import Data.List ( findIndex, intersperse )
import System ( getArgs )


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
    isEndGame str = or $ map (== head str) "WLT"
    sqr i = fromIntegral $ i^(2::Int)
    incFreqs i xs = let mi = findIndex (\(i', _) -> i' == i) xs
                    in case mi of
                         Just hi -> let (a, b) = splitAt hi xs
                                    in a ++
                                       (\(i', c) -> (i', c + 1)) (head b) :
                                       tail b
                         Nothing -> xs


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
    lFs = zip [38..tOCount] zeros
    zeros = repeat 0
    tOCount = 2000


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


data RawStats = RawStats { winN :: !Int
                         , winMin :: !Int
                         , winMax :: !Int
                         , winSumX :: !Integer
                         , winSumXX :: !Integer
                         , winFreqs :: [(Int,Int)]
                         , lossN :: !Int
                         , lossMin :: !Int
                         , lossMax :: !Int
                         , lossSumX :: !Integer
                         , lossSumXX :: !Integer
                         , lossFreqs :: [(Int,Int)]
                         , timeoutN :: !Int
                         } deriving (Show)


data GameEnds = Win Int
              | Loss Int
              | Timeout
                deriving ( Read
                         , Show )
