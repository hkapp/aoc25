#!/bin/bash

ghc src/Day$1.hs -outputdir out -o out/Day$1 -prof -fprof-auto -rtsopts && time out/Day$1 +RTS -p
