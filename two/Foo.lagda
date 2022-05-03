\begin{code}[hide]
module Foo where

open import Data.List using (List; _++_; [_] ; [];  _∷_ )

data Nat : Set where
  zero : Nat
  suc : Nat → Nat

\end{code}

\begin{code}


data tSymb : Set where
  base : Nat → tSymb
  ~ : tSymb → tSymb
  _\\_ : tSymb → tSymb → tSymb
  _//_ : tSymb → tSymb → tSymb

\end{code}
