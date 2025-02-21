#!/bin/bash
nodemon --watch src --watch app -e hs --exec "cabal run"
