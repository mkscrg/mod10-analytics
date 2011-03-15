-- | The Params module uses the System.Console.GetOpt library to parse
-- command-line arguments for Mod10.
module Simulation.Params ( getParams
                         , RunParams(..)
                         ) where


import System ( getArgs, exitWith
              , ExitCode(..) )
import System.Console.GetOpt ( getOpt, usageInfo
                             , ArgDescr(..), ArgOrder(..), OptDescr(..) )
import System.IO ( hPutStrLn, openFile, stderr, stdout
                 , Handle, IOMode(..) )
import System.IO.Error ( ioeGetErrorString )


-- | Get the command-line arguments and validate them if reasonable. Print help
-- info if requested, and exit with error if arguments were unreasonable.
getParams :: IO RunParams
getParams = do
    argv <- getArgs
    case getOpt Permute options argv of
      (_, _, errs)  | not (null errs)          -> die errs
      (flags, _, _) | Help `elem` flags        -> help
      (flags, _, _)                            -> validate flags


-- | Convert a reasonable set of Flags into a RunParams record, using default
-- values where necessary. Exit with error if an invalid filename was given.
validate :: [Flag] -> IO RunParams
validate flags = do
    let v = if Verbose `elem` flags
              then True
              else False
    o <- oDef flags
    return $ RunParams { nGames = nDef flags
                       , outH = o
                       , verbose = v }
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


-- | Exit with an error after printing the given string, and the usage info.
die :: [String] -> IO b
die errs = dump (concat errs ++ info) >> exitWith (ExitFailure 1)


-- | Print the usage info and exit normally.
help :: IO b
help = dump info >> exitWith ExitSuccess


-- | Usage info, generated from the header and the option definitions.
info :: String
info = usageInfo header options


-- | Print to stderr.
dump :: String -> IO ()
dump = hPutStrLn stderr


-- | Header string, showing the command syntax.
header :: String
header = "Mod10 [-h/--help] [-o/--outfile fname] [-n/--ngames N] " ++ 
               "[-v/--verbose]"


-- | Options definition.
options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)
                   "Prints this help message"
          , Option ['o'] ["outfile"] (ReqArg (\s -> OutFile s) "fname")
                   "Specify where output is written (default stdout)"
          , Option ['n'] ["ngames"] (ReqArg (\s -> NGames (read s)) "N")
                   "Specify how many games are played (default 1)"
          , Option ['v'] ["verbose"] (NoArg Verbose)
                   "Prints full game rounds" ]


data RunParams =
    RunParams { nGames :: Int    -- ^ Number of games unfinished
              , outH :: Handle   -- ^ Handle of output location
              , verbose :: Bool  -- ^ Whether to print GameState at each turn
              } deriving (Show)


data Flag = Help
          | OutFile FilePath  -- ^ User-spec'd output path
          | NGames Int        -- ^ User-spec'd number of games
          | Verbose           -- ^ User-spec'd verbosity
            deriving (Eq)

