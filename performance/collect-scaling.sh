#!/bin/bash
#SBATCH --nodes=64
#SBATCH --time=20:00
#SBATCH --constraint=cpu
#SBATCH --account=m4746
#SBATCH --qos=regular

# This needs to be run on an allocation with at least 64 nodes

make scaling.exe

echo "NodeCount,RunsPerNode,Time" > weak-scaling.csv
echo "NodeCount,RunsPerNode,Time" > strong-scaling.csv

for nodeCount in 1 2 4 8 16 32 64; do
  ./scaling.exe -nl$nodeCount --numRuns 512 >> strong-scaling.csv
  weakNumRuns=$(echo 512 * $nodeCount | bc)
  ./scaling.exe -nl$nodeCount --numRuns $weakNumRuns >> weak-scaling.csv
done
