import System.IO


main :: IO [String]
main = do
    inh <- openFile "100run.txt" ReadMode
    inpStr <- hGetContents inh
    let gs = games inpStr
    return gs


getRawStats :: String -> RawStats
getRawStats s = RawStats { wins = 5, losses = 10 }


games :: String -> [String]
games s = let (g, s') = break (== '%') s
          in g : case s' of
                   "%\n" -> []
                   (_:s'') -> games (drop 1 s'')  -- drop % and \n


linesWithTerm :: String -> [String]
linesWithTerm s = break lines s


data RawStats = RawStats { wins :: Int
                         , losses :: Int
                         } deriving (Show)
