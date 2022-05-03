\begin{code}[hide]
module Foo1 where

-- open import Data.List using (List; _++_; [_] ; [];  _∷_ ) renaming (_∷_ to _,_)
open import Data.List renaming (_∷_ to _,_)

data Nat : Set where
  zero : Nat
  suc : Nat → Nat

data tSymb : Set where
  base : Nat → tSymb
  ~ : tSymb → tSymb
  _\\_ : tSymb → tSymb → tSymb
  _//_ : tSymb → tSymb → tSymb
\end{code}

\begin{code}

Ctx : Set
Ctx = List tSymb

data _=>_ : Ctx → tSymb → Set where
  id-axiom : (x : tSymb) → [ x ] => x
  \\r : (Γ : Ctx )(x y : tSymb)
        → ( x , Γ) => y
        → Γ => ( x \\ y )
  \\l : (Δ Δ' Γ : Ctx )(x y z : tSymb)
        → (Δ ++ [ y ] ++ Δ') => z
        → Γ => x
        -----------------------------
        → (Δ ++ Γ ++ [ x \\ y ] ++ Δ')
          => z

\end{code}
