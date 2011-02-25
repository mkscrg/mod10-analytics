module Main where


import Data.Array ( listArray )
import Data.List ( stripPrefix )
import Data.Maybe ( mapMaybe )
import System ( getArgs )
import System.Exit ( ExitCode )

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.MultiPlot as MultiPlot
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame as Frame


main :: IO ()
main = do
    argv <- getArgs
    let statfile = head argv
    stats <- readFile statfile
    let runs = chunks stats
    let plots = map makePlot runs
    sequence_ plots


makePlot :: [String] -> IO ExitCode
makePlot run = Plot.plot term $ MultiPlot.simpleFromPartArray $
               listArray ((0::Int, 0::Int), (1, 0)) $
               map MultiPlot.partFromFrame frames
  where
    frames = zipWith Frame.cons options $
             map (Plot2D.list Graph2D.lines) freqs
    options = map (\o -> o genOpt) [topOpt, botOpt]
    topOpt = Opts.title figtitle .
             Opts.add (Opt.custom "bmargin") ["0.0"] .
             Opts.yLabel "winning" .
             Opts.remove Opt.xTicks
    botOpt = Opts.add (Opt.custom "tmargin") ["0.0"] .
             Opts.yLabel "losing" .
             Opts.add Opt.xTicks ["nomirror"]
    genOpt = Opts.xRange2d (0,1500) $
             Opts.remove Opt.yTicks $
             Opts.remove Opt.key Opts.deflt
    term = PNG.fontLarge $
           PNG.cons fname
    figtitle = "# of Rounds in Winning and Losing Games (" ++
               last (words (run !! 1)) ++ ")"
    fname = plotFilename $ head run
    freqs = map (\lab -> getFreqs $ head $ mapMaybe (stripPrefix lab) run)
                dataLabels
    dataLabels = ["    win freq.s: ", "   loss freq.s: "]


getFreqs :: String -> [(Int, Int)]
getFreqs = read


chunks :: String -> [[String]]
chunks str = filter (not . null) $ sepByBlank $ lines str
  where
    sepByBlank [""]  = []
    sepByBlank lns = let (chunk, lns') = break (== "") lns
                     in chunk : case lns' of
                          []        -> []
                          (_:lns'') -> sepByBlank lns''


plotFilename :: String -> String
plotFilename runFName = fst (break (== '.') runFName) ++ ".png"
