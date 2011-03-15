# Makefile for mod10-analytics

HC = ghc
HC_OPTS = --make -Wall -Werror

simulate :
	$(HC) $(HC_OPTS) simulate.hs

analyze :
	$(HC) $(HC_OPTS) analyze.hs

clean :
	rm *.hi *.o
	rm Simulation/*.hi Simulation/*.o
	rm Analysis/*.hi Analysis/*.o
	rm simulate
	rm analyze

clean-plots :
	rm *.gp
	rm *.csv
