module Main where


import Data.Foldable ( foldl' )
import Data.List ( intersperse
                 , isPrefixOf )
import System ( getArgs )
import System.IO ( hClose
                 , hGetContents
                 , openFile
                 , IOMode(..) )


main :: IO ()
main = do
    argv <- getArgs
    inH <- openFile (head argv) ReadMode
    raw <- hGetContents inH
    let rs = getRawStats raw
    let cs = getCleanStats rs
    putStrLn $ show cs ++ "\n"
    hClose inH


getRawStats :: String -> RawStats
getRawStats raw = collect (filter isEndGame (lines raw)) $
    RawStats { winCounts = []
             , lossCounts = []
             , timeouts = 0 }
  where
    collect []       sts = sts
    collect (rl:rls) sts =
        case read rl of
          Win i   -> collect rls
                          sts { winCounts = i : winCounts sts }
          Loss i  -> collect rls
                          sts { lossCounts = i : lossCounts sts }
          Timeout -> collect rls
                          sts { timeouts = 1 + timeouts sts }
    isEndGame ln | "Win"     `isPrefixOf` ln = True
    isEndGame ln | "Loss"    `isPrefixOf` ln = True
    isEndGame ln | "Timeout" `isPrefixOf` ln = True
    isEndGame _                              = False


getCleanStats :: RawStats -> CleanStats
getCleanStats RawStats {winCounts=winC, lossCounts=lossC, timeouts=timeN} = 
    CleanStats { count = n
               , winFreq = freq winN n
               , winMeanCount = winMeC
               , winStdDevCount = stdDev winC winN winMeC
               , lossFreq = freq lossN n
               , lossMeanCount = lossMeC
               , lossStdDevCount = stdDev lossC lossN lossMeC
               , timeoutFreq = freq timeN n }
  where
    winMeC = mean winC winN
    lossMeC = mean lossC lossN
    winN = length winC
    lossN = length lossC
    n = winN + lossN + timeN


stdDev :: [Int] -> Int -> Double -> Double
stdDev xis n xbar = (**) (1 / (fromIntegral n - 1) *
                          (sum' (map sqdiff xis))) 0.5
  where
    sqdiff c = (fromIntegral c - xbar) ** 2


freq :: Int -> Int -> Double
freq a b = fromIntegral a / fromIntegral b


mean :: [Int] -> Int -> Double
mean xis n = fromIntegral (sum' xis) / fromIntegral n


sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0


data GameEnds = Win Int
              | Loss Int
              | Timeout
                deriving ( Read
                         , Show )


data CleanStats = CleanStats { count :: Int
                             , winFreq :: Double
                             , winMeanCount :: Double
                             , winStdDevCount :: Double
                             , lossFreq :: Double
                             , lossMeanCount :: Double
                             , lossStdDevCount :: Double
                             , timeoutFreq :: Double }

instance Show CleanStats where
    show sts = concat $ intersperse "\n" $
               [ "games played: " ++ show (count sts)
               , "win frequency: " ++ show (winFreq sts * 100) ++ "%"
               , "winning mean count: " ++ show (winMeanCount sts)
               , "winning std. dev. count: " ++ show (winStdDevCount sts)
               , "loss frequency: " ++ show (lossFreq sts * 100) ++ "%"
               , "losing mean count: " ++ show (lossMeanCount sts)
               , "losing std. dev. count: " ++ show (lossStdDevCount sts)
               , "timeout frequency: " ++ show (timeoutFreq sts * 100) ++ "%" ]


data RawStats = RawStats { winCounts :: [Int]
                         , lossCounts :: [Int]
                         , timeouts :: Int
                         } deriving (Show)
