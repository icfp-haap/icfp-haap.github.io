#!/bin/bash

ghc -fhpc $1.hs -main-is $1 -o $1 &&

for file in ../tests/$1/*.in; do cat $file | ./$1 > /dev/null; done &&

hpc markup $1 &&
hpc report $1 &&

xdg-open hpc_index.html
