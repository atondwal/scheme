#!/bin/bash
for f in tests/*.ss
do
  echo "Processing $f file..."
  expected=$(cat $f | scm)
  actual=$(./main `cat $f`)
  echo "Actual output = $actual"
  subtestcounter=0
  for l in $expected
  do
    echo "Output = $l"
    subtestcounter=$((subtestcounter+1))
  done
done
