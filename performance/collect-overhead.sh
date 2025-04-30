#!/bin/bash
# Creates a csv file with timing results for the overhead comparison
set -x
set -e

make memcpy-overhead.exe fortran-overhead.exe chapel-overhead.exe

outfile="overhead.csv"

echo "Version,NumRuns,IO,ConfigFile,Init,SurfaceSetting,SurfaceCompute,InteriorSetting,InteriorCompute,CopyBack" > $outfile

for val in 50 100 200 400 800; do
  ./fortran-overhead.exe $val 2> /dev/null 1>> $outfile
  ./chapel-overhead.exe -nl1 --numRuns=$val >> $outfile
  ./memcpy-overhead.exe -nl1 --numRuns=$val >> $outfile
done