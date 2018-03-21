#!/bin/bash

ghc -fhpc Sokoban.hs -main-is Sokoban -static -O2 -threaded -o Soko
