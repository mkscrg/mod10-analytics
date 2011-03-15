-- | The 'Analysis.Params' module uses the 'System.Console.GetOpt' library to
-- parse command-line options for the @analyze@ executable.
module Analysis.Params ( getParams
                      , RunParams(..)
                      ) where


import Data.Maybe ( mapMaybe )
import System ( getArgs, exitWith
              , ExitCode(..) )
import System.Console.GetOpt ( getOpt, usageInfo
                             , ArgDescr(..), ArgOrder(..), OptDescr(..) )
import System.IO ( hPutStrLn, openFile, stderr, stdin, stdout
                 , Handle, IOMode(..) )
import System.IO.Error ( ioeGetErrorString )


-- | Get the command-line arguments and validate them if reasonable. Print help
-- info if requested, and exit with error(s) if the arguments were unreasonable.
getParams :: IO RunParams
getParams = do
    argv <- getArgs
    case getOpt Permute options argv of
      (_, _, errs)  | not (null errs)   -> die errs
      (flags, _, _) | Help `elem` flags -> help
      (flags, _, _)                     -> validate flags


-- | Convert a reasonable set of 'Flag's into a 'RunParams' record, using
-- default values where necessary. Exit with error if an invalid filename was
-- given.
validate :: [Flag] -> IO RunParams
validate flags = do
    i <- case mapMaybe (\flag -> case flag of
                                   InFile fpath -> Just fpath
                                   _            -> Nothing) flags of
           (fpath:_) -> valPath fpath ReadMode
           []        -> return stdin
    o <- case mapMaybe (\flag -> case flag of
                                   OutFile fpath -> Just fpath
                                   _             -> Nothing) flags of
           (fpath:_) -> valPath fpath WriteMode
           []        -> return stdout
    let p = Plots `elem` flags
    return $ RunParams { inH = i
                       , outH = o
                       , plots = p }
  where
    valPath fpath ioMode = catch (openFile fpath ioMode) $
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
header = "analyze [-h/--help] [-i/--infile fpath] [-o/--outfile fpath] " ++
                 "[-p/--plots]"


-- | Options definition.
options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)
                   "Prints this help message"
          , Option ['i'] ["infile"] (ReqArg (\s -> InFile s) "fpath")
                   "Specify where input is read (default stdin)"
          , Option ['o'] ["outfile"] (ReqArg (\s -> OutFile s) "fpath")
                   "Specify where output is written (default stdout)"
          , Option ['p'] ["plots"] (NoArg Plots)
                   "Makes plots of win/loss frequencies using gnuplot" ]


-- | Record which governs how @analyze@ runs
data RunParams =
    RunParams { inH :: Handle
              -- ^ Handle of input path
              , outH :: Handle
              -- ^ Handle of output path
              , plots :: Bool
              -- ^ Whether to make plots of win/loss freqs using gnuplot
              }


data Flag = Help
          | InFile FilePath   -- ^ User-spec'd input path
          | OutFile FilePath  -- ^ User-spec'd output path
          | Plots             -- ^ Generate plots of input data
            deriving (Eq)

