{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Diag (diagMain) where

import Diagrams.Backend.SVG.CmdLine

import Diagrams.Prelude

import Diagrams.TwoD.Factorization

example = fdGridList 6 # center # pad 1.05

diagMain = mainWith (example :: Diagram B)
