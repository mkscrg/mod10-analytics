#!/bin/bash

# Clean project directories of compiled and intermediate files.

rm *.hi *.o *.gp simulate analyze
cd Simulation
rm *.hi *.o
cd ..
cd Analysis
rm *.hi *.o
