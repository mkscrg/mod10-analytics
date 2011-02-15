module Main (main) where

import System (getArgs, exitWith, ExitCode(..))
import System.Console.GetOpt
import System.IO
import System.Random (newStdGen)

import Card
import Mechanics


main :: IO ()
main = do
    mode <- getMode
    case mode of
      Interact -> iGame
      Output n fname -> do
          h <- openFile fname WriteMode
          nGames n h


iGame :: IO ()
iGame = do
    g <- newGame
    _ <- playGame stdout True g
    return ()


nGames :: Int -> Handle -> IO ()
nGames 0 _ = return ()
nGames n h = do
    g <- newGame
    _ <- playGame h False g
    nGames (n - 1) h


playGame :: RunMode -> GameState -> IO GameState
playGame mode g = do
    let (h, i) = case mode of
                   Interact    -> (stdout, True)
                   Output _ h' -> (h', False)
    let g' = g { ctr = ctr g + 1 }
    hPrint h g'
    _ <- if not i
           then hPutStr h "\n"
           else return ()
    if null (deck g')
      then do hPutStr h ("L " ++ show (ctr g') ++ "%\n")
              return g'
      else if ctr g > 1500
             then do hPutStr h ("T " ++ show (ctr g') ++ "%\n")
                     return g'
             else do
                 _ <- if i
                        then getLine
                        else return ""
                 case advance (playMoves g') of
                   Just g'' -> playGame h i (feed g'')
                   Nothing -> do hPutStr h ("W " ++ show (ctr g') ++ "%\n")
                                 return g'


newGame :: IO GameState
newGame = do
    seed <- newStdGen
    return (feed (feed14 (GameState { deck = shuffle seed newdeck
                                    , stacks = replicate 7 []
                                    , csi = 0
                                    , ctr = 0 })))


playMoves :: GameState -> GameState
playMoves g = case play g of
              Just g' -> playMoves g'
              Nothing -> g


feed14 :: GameState -> GameState
feed14 g = feed' 14 g
  where
    feed' :: Int -> GameState -> GameState
    feed' 0 g'                  = g'
    feed' j g' = feed' (j - 1) (inc (feed g'))
    inc g' = g' { csi = (csi g' + 1) `mod` length (stacks g') }


getMode :: IO RunMode
getMode = do
    argv <- getArgs
    case getOpt Permute options argv of
      (_, _, errs) | not (null errs)         -> die errs
      (opts, _, _) | Help `elem` opts        -> help
      (opts, _, _) | Interactive `elem` opts -> return Interact
      ([NGames i], [fname], _)               -> return $ Output i $
                                                  openFile fname WriteMode
      ([], [fname], _)                       -> return $ Output 1 $
                                                  openFile fname WriteMode
      _                                      -> help
  where
    die errs = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
    help = dump info >> exitWith ExitSuccess
    dump = hPutStrLn stderr
    info = usageInfo header options
    options = [ Option ['h'] ["help"] (NoArg Help)
                       "Prints this help message"
              , Option ['i'] ["interactive"] (NoArg Interactive)
                       ("Watch a game with prompts between turns \n" ++
                        "  (-n and filename are ignored)")
              , Option ['n'] ["ngames"] (ReqArg (\s -> NGames (read s)) "N")
                       "Specify how many games are played (default 1)" ]
    header = "Usage: testargs [-h/--help] [-i] [-n/--ngames N] filename"


data RunMode = Interact
             | Output Int Handle
               deriving (Show)

data Flag = Help
          | Interactive
          | NGames Int
            deriving (Eq, Show)
