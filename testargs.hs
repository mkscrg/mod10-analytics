import System (getArgs, exitWith, ExitCode(..))
import System.IO

getMode :: IO RunMode
getMode = do
    argv <- getArgs
    case getOpt Permute options argv of
      (_, _, errs) | not (null errs)         -> die errs
      (opts, _, _) | Help `elem` opts        -> help
      (opts, _, _) | Interactive `elem` opts -> return Interact
      ([NGames i], [fname], _)               -> return (Output i fname)
      ([], [fname], _)                       -> return (Output 1 fname)
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
             | Output Int String
               deriving (Show)

data Flag = Help
          | Interactive
          | NGames Int
            deriving (Eq, Show)

