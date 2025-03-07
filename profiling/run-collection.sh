#!/bin/bash
set -x

outfile=""
if [ $# -eq 0 ]; then
  outfile="timing.csv"
else
  outfile=$1
fi

make clean
make chapelTest.exe
make fortranTest.exe



echo "Version,N,Time (s)" > $outfile
for n in 10 50 100 500 1000; do
  tmp=$(mktemp)
  /usr/bin/time -f "%e" ./fortranTest.exe allocatable $n >> $outfile 2> $tmp
  t=$(cat $tmp)
  echo $n,$t >> $outfile

  ./chapelTest.exe -nl1 --n=$n --dataParTasksPerLocale=1 >> $outfile
  ./chapelTest.exe -nl1 --n=$n >> $outfile
  ./chapelTest.exe -nl2 --n=$n >> $outfile
  ./chapelTest.exe -nl4 --n=$n >> $outfile
done
