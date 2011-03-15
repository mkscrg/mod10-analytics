# Makefile for mod10-analytics

HC = ghc
HC_OPTS = --make -Wall -Werror

simulate : simulate.hs Simulation/Params.hs Simulation/Mechanics.hs Simulation/Card.hs
	$(HC) $(HC_OPTS) simulate.hs

analyze : analyze.hs Analysis/Params.hs Analysis/Scrape.hs Analysis/Plots.hs
	$(HC) $(HC_OPTS) analyze.hs

clean :
	rm *.hi *.o
	rm simulate
	rm Simulation/*.hi Simulation/*.o
	rm analyze
	rm Analysis/*.hi Analysis/*.o

clean-plots :
	rm *.gp
	rm *.csv
