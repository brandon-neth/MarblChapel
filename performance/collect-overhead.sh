#!/bin/bash
# Creates a csv file with timing results for the overhead comparison
set -x
set -e

make overhead

outfile="overhead.csv"

echo "Version,NumRuns,IO,SettingFile,Init,SurfaceFlux,InteriorTendency,Compute" > $outfile

for val in 1 5 10 50 100; do
  ./fortran-overhead.exe $val 2> /dev/null 1>> $outfile
  ./chapel-overhead.exe -nl1 --numRuns=$val >> $outfile
done