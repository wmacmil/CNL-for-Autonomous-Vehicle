\begin{code}

open import F

plus : Nat → Nat → Nat
plus zero b = b
plus (suc a) b = suc (plus a b)

\end{code}
