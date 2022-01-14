
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module LTL where

data Atomic where

-- syntax of a LTL formulag
data Phi where
  Atom                :: Atomic -> Phi
  Bottom, Top         :: Phi
  Negate              :: Phi -> Phi
  Join, Meet, Implies :: Phi -> Phi -> Phi
  X, F, G             :: Phi -> Phi
  U, W, R             :: Phi -> Phi -> Phi

