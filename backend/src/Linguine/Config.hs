{-# LANGUAGE CPP #-}

module Linguine.Config (productionMode) where

productionMode :: Bool
#ifdef PROD
productionMode = True
#else
productionMode = False
#endif
