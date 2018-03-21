#!/bin/bash

# find -name '*.hs' | xargs graphmod -q | xdot -

find -name '*.hs' | xargs graphmod -q | dot -Tsvg >modules.svg &&
xdg-open modules.svg
