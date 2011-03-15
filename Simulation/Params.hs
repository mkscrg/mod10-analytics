-- | The 'Simulation.Params' module uses the 'System.Console.GetOpt' library to
-- parse command-line options for the @simulate@ executable.
module Simulation.Params ( getParams
                         , RunParams(..)
                         ) where


import Data.Maybe ( mapMaybe )
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


-- | Turn a reasonable set of 'Flag's into a 'RunParams' record, using default
-- values where necessary. Exit with error if an invalid filename was given.
validate :: [Flag] -> IO RunParams
validate flags = do
    let f = Filter `elem` flags
    let n = case mapMaybe (\flag -> case flag of
                                      NGames n' -> Just n'
                                      _         -> Nothing) flags of
              (n':_) -> n'
              []     -> 1
    o <- case mapMaybe (\flag -> case flag of
                                   OutFile fpath -> Just fpath
                                   _             -> Nothing) flags of
           (fpath:_) -> valPath fpath
           []        -> return stdout
    let v = Verbose `elem` flags
    return $ RunParams { filt = f
                       , nGames = n
                       , outH = o
                       , verbose = v }
  where
    valPath fpath = catch (openFile fpath WriteMode) $
                          \err -> die [ioeGetErrorString err ++ "\n"]


-- | Exit with an error after printing the given string, and the usage info.
die :: [String] -> IO b
die errs = dump (concat errs ++ info) >> exitWith (ExitFailure 1)


-- | Print the usage info and exit normally.
help :: IO b
help = dump info >> exitWith ExitSuccess


-- | Usage info, generated from the header and the option definitions.
info :: String
info = usageInfo header options


-- | Print to 'System.IO.stderr'.
dump :: String -> IO ()
dump = hPutStrLn stderr


-- | Header string, showing the command syntax.
header :: String
header = "simulate [-h/--help] [-f/--filter] [-n/--ngames N] " ++
                  "[-o/--outfile fname] [-v/--verbose]"


-- | Options definition.
options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)
                   "Prints this help message"
          , Option ['f'] ["filter"] (NoArg Filter)
                   "Timeout if # of stacks hasn't decreased after first row"
          , Option ['n'] ["ngames"] (ReqArg (\s -> NGames (read s)) "N")
                   "Specify how many games are played (default 1)"
          , Option ['o'] ["outfile"] (ReqArg (\s -> OutFile s) "fname")
                   "Specify where output is written (default stdout)"
          , Option ['v'] ["verbose"] (NoArg Verbose)
                   "Prints full game rounds" ]


-- | Record which governs how @simulate@ runs
data RunParams =
    RunParams { filt :: Bool
              -- ^ Whether to filter games after first triplets are dealt
              , nGames :: Int
              -- ^ Number of games unfinished
              , outH :: Handle
              -- ^ Handle of output location
              , verbose :: Bool
              -- ^ Whether to print GameState at each turn
              }


data Flag = Help
          | Filter            -- ^ Use filtering
          | NGames Int        -- ^ Set number of games
          | OutFile FilePath  -- ^ Set output path
          | Verbose           -- ^ Print verbose output
            deriving (Eq)
