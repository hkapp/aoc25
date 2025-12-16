#!/bin/bash

ghc src/Day$1.hs -outputdir out -o out/Day$1 && time out/Day$1
