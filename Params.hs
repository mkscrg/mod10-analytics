module Params ( getParams
              , RunParams(..)
              ) where


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
      (flags, _, _)                            -> validate flags


validate :: [Flag] -> IO RunParams
validate flags = do
    let q = if Quiet `elem` flags
              then True
              else False
    o <- oDef flags
    return $ RunParams { ngames = nDef flags
                       , outh = o
                       , quiet = q }
  where
    oDef []     = return stdout
    oDef (x:xs) = case x of
                    OutFile fname -> catch (openFile fname WriteMode) $
                                     \err -> die
                                         [ioeGetErrorString err ++ "\n"]
                    _             -> oDef xs
    nDef []     = 1
    nDef (x:xs) = case x of
                    NGames i -> i
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
header = "Mod10 [-h/--help] [-o/--outfile fname] [-n/--ngames N] [-q/--quiet]"


options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)
                   "Prints this help message"
          , Option ['o'] ["outfile"] (ReqArg (\s -> OutFile s) "fname")
                   "Specify where output is written (default stdout)"
          , Option ['n'] ["ngames"] (ReqArg (\s -> NGames (read s)) "N")
                   "Specify how many games are played (default 1)"
          , Option ['q'] ["quiet"] (NoArg Quiet)
                   "Prints only results, not full game rounds" ]


data RunParams = RunParams { ngames :: Int
                           , outh :: Handle
                           , quiet :: Bool
                           } deriving (Show)


data Flag = Help
          | OutFile FilePath
          | NGames Int
          | Quiet
            deriving (Eq)

