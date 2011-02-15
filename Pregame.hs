module Pregame ( getParams
               , RunParams(..)
               ) where


import Data.Maybe ( fromMaybe )
import System ( getArgs
              , exitWith
              , ExitCode(..) )
import System.Console.GetOpt ( getOpt
                             , usageInfo
                             , ArgDescr(..)
                             , ArgOrder(..)
                             , OptDescr(..) )
import System.IO ( hPutStrLn
                 , openFile
                 , stderr
                 , stdout
                 , Handle
                 , IOMode(..) )
import System.IO.Error ( ioeGetErrorString )


getParams :: IO RunParams
getParams = do
    argv <- getArgs
    case getOpt Permute options argv of
      (_, _, errs)  | not (null errs)          -> die errs
      (flags, _, _) | Help `elem` flags        -> help
      (flags, _, _) | Interactive `elem` flags ->
          return $ RunParams { ngames = 1
                             , outh = stdout
                             , quiet = False }
      (flags, fname:_, _)                      -> validate flags fname
      (_, [], _)                               -> die ["out_fname not given\n"]


validate :: [Flag] -> FilePath -> IO RunParams
validate flags fname = do
    h <- catch (openFile fname WriteMode) $
               \err -> die [ioeGetErrorString err ++ "\n"]
    let q = if Quiet `elem` flags
              then True
              else False
    return $ RunParams { ngames = fromMaybe 1 $ nDef flags
                       , outh = h
                       , quiet = q }
  where
    nDef []     = Nothing
    nDef (x:xs) = case x of
                    NGames i -> Just i
                    _        -> nDef xs


die :: [String] -> IO b
die errs = dump (concat errs ++ info) >> exitWith (ExitFailure 1)


help :: IO b
help = dump info >> exitWith ExitSuccess


info :: String
info = usageInfo header options


dump :: String -> IO ()
dump = hPutStrLn stderr


header :: String
header = "Mod10 [-h/--help] [-i] [-n/--ngames N] out_fname"


options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)
                   "Prints this help message"
          , Option ['i'] ["interactive"] (NoArg Interactive)
                   $ "Watch a game with prompts between turns\n" ++
                     "  (-n, -q, and out_fname are ignored)"
          , Option ['n'] ["ngames"] (ReqArg (\s -> NGames (read s)) "N")
                   "Specify how many games are played (default 1)"
          , Option ['q'] ["quiet"] (NoArg Quiet)
                   "Prints only Win/Loss/Timeout, not game rounds" ]


data RunParams = RunParams { ngames :: Int
                           , outh :: Handle
                           , quiet :: Bool }


data Flag = Help
          | Interactive
          | NGames Int
          | Quiet
            deriving (Eq)

