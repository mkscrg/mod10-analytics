-- | The 'Analysis.Plots' module defines the 'makePlot' function, which gives
-- the @analyze@ executable the ability to plot its input data, saving the
-- graphic as "plot.png". The 'Graphics.Gnuplot' library is required. A working
-- installation of @gnuplot@, compiled to support the .png terminal, is
-- required for full functionality; without such an installation, @analyze@
-- will still produce .csv files of the plotted data.
module Analysis.Plots ( makePlot ) where


import Data.Array ( listArray )
import System.Exit ( ExitCode )

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.MultiPlot as MultiPlot
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame as Frame

import Analysis.Scrape ( RawStats(..) )


-- | Construct an IO action for the production of the .png plot.
makePlot :: RawStats     -- ^ includes the relevant frequency data
         -> Int          -- ^ the # of games played, for the figure title
         -> IO ExitCode
makePlot stats n = Plot.plot term $ MultiPlot.simpleFromPartArray $
                      listArray ((0::Int, 0::Int), (1, 0)) $
                                map MultiPlot.partFromFrame frames
  where
    frames = zipWith Frame.cons options $
                map (Plot2D.list Graph2D.lines) [wins stats, losses stats]
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
    term = PNG.fontLarge $ PNG.cons "plot.png"
    figtitle = "# of Rounds in Winning and Losing Games (" ++ show n ++ ")"
