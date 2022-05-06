\begin{code}[hide]
module Model where

open import Support

{-
Refactored so-as to allow for easier (more infomrative) proofs
Originally had
L : State → 𝑃 Atom
-}
\end{code}
\begin{code}
record 𝑀 (Atom : Set) : Set1 where
  field
    State : Set
    _⟶_ : rel State
    relSteps : relAlwaysSteps _⟶_
    L : State → Atom → Set
    -- L'' : Decidable L'
\end{code}
