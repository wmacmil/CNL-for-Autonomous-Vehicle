\begin{code}[hide]
module twin-primes where

open import Data.Nat renaming (_+_ to _∔_)
open import Data.Product using (Σ; _×_; _,_; proj₁; proj₂; ∃; Σ-syntax; ∃-syntax)
open import Data.Sum renaming (_⊎_ to _+_)
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; trans; sym; cong; cong-app; subst)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)

is-prime : ℕ → Set
is-prime n =
  (n ≥ 2) ×
  ((x y : ℕ) → x * y ≡ n → (x ≡ 1) + (x ≡ n))
\end{code}

\begin{code}
twin-prime-conjecture : Set
twin-prime-conjecture = (n : ℕ) → Σ[ p ∈ ℕ ] (p ≥ n)
  × is-prime p
  × is-prime (p ∔ 2)
\end{code}

