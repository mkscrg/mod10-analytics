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
makePlot run = Plot.plot term $ MultiPlot.simpleFromFrameArray $
               listArray ((0::Int, 0::Int), (1, 0)) frames
  where
    frames = zipWith Frame.cons options $
             map (Plot2D.list Graph2D.listLines)
             [winHist, lossHist]
    options = map (\o -> o genOpt) [topOpt, botOpt]
    topOpt = Opts.add (Opt.custom "bmargin") ["0.0"] .
             Opts.remove Opt.xTicks
    botOpt = Opts.add (Opt.custom "tmargin") ["0.0"] .
             Opts.add Opt.xTicks ["nomirror"]
    genOpt = Opts.remove Opt.yTicks $
             Opts.remove Opt.key Opts.deflt
    term = PNG.fontSmall $
           PNG.cons fname
    fname = plotFilename $ head run
    winHist = head $ mapMaybe (stripPrefix winLabel) run
    lossHist = head $ mapMaybe (stripPrefix lossLabel) run
    winLabel = "win histogram: "
    lossLabel = "loss histogram: "


chunks :: String -> [[String]]
chunks str = sepByBlank $ lines str
  where
    sepByBlank [""]  = []
    sepByBlank lns = let (chunk, rest) = break (== "") lns
                     in chunk : sepByBlank rest


plotFilename :: String -> String
plotFilename runFName = fst (break (== '.') runFName) ++ ".png"


--makeplot :: RawStats -> FilePath -> IO ExitCode
--makeplot stats statfile =
--    Plot.plot term $ MultiPlot.simpleFromFrameArray $
--    listArray ((0::Int, 0::Int), (1, 0)) frames
--  where
--    frames = zipWith Frame.cons options $
--             map (Plot2D.list Graph2D.listLines)
--             [lossHist stats, winHist stats]
--    options = map (\o -> o genOpt) [topOpt, botOpt]
--    topOpt = Opts.add (Opt.custom "bmargin") ["0.0"] .
--             Opts.remove Opt.xTicks
--    botOpt = Opts.add (Opt.custom "tmargin") ["0.0"] .
--             Opts.add Opt.xTicks ["nomirror"]
--    genOpt = Opts.remove Opt.yTicks $
--             Opts.remove Opt.key Opts.deflt
--    term = PNG.fontSmall $
--           PNG.cons $ fst (break (== '.') statfile) ++ ".png"
