\begin{code}[hide]
module Syntaxx (Atom : Set) where
-- Think Atom =FinSet
\end{code}
\begin{code}[hide]
data ϕ : Set where
  atom        : Atom → ϕ
  ⊥ ⊤         : ϕ
  ¬_          : ϕ → ϕ
  _∨_ _∧_ _⇒_ : ϕ → ϕ → ϕ
  X F G       : ϕ → ϕ
  _U_ _W_ _R_ : ϕ → ϕ → ϕ
\end{code}
\begin{code}[hide]
open import Data.List

-- that it automatically satisfies true, so if there are no location propositions
-- a trace is just a list of atoms
-- coverage versus surveillance patterns

-- data atoms : Set where
--   corner : atoms
--   person : atoms
--   home   : atoms
--   cafe   : atoms

\end{code}
\begin{code}
visit : List Atom → ϕ
visit [] = ⊤
visit (l ∷ ls) = (F (atom l)) ∧ visit ls

sequentialVisit : List Atom → ϕ
sequentialVisit [] = ⊤
sequentialVisit (l ∷ ls) = F (atom l ∧ sequentialVisit ls)

predecessorPrecedesSuccessor : List Atom → ϕ
predecessorPrecedesSuccessor [] = ⊤
predecessorPrecedesSuccessor (l ∷ []) = ⊤
predecessorPrecedesSuccessor (l ∷ l' ∷ ls) =
  ((¬ atom l') U atom l) ∧ predecessorPrecedesSuccessor (l' ∷ ls)

orderedVisit : List Atom → ϕ
orderedVisit ls = sequentialVisit ls ∧ predecessorPrecedesSuccessor ls
\end{code}
\begin{code}[hide]

-- strictness condition
-- don't visit a location twice
-- probably not realistic for our example
-- everything should also depend on the location of the vehicle
-- past temporal operators should be used to judge the success of a mission
strictSeq : List Atom → ϕ
strictSeq [] = ⊤
strictSeq (l ∷ []) = ⊤
strictSeq (lᵢ ∷ lᵢ₊₁ ∷ ls) = ((¬ atom lᵢ₊₁) U (atom lᵢ ∧ X ((¬ atom lᵢ) U atom lᵢ₊₁))) ∧ strictSeq (lᵢ₊₁ ∷ ls)

strictOrderedVisit : List Atom → ϕ
strictOrderedVisit ls = orderedVisit ls ∧ strictSeq ls

-- define a satisfaction for a trace -- only care about the finite case, but really we should generalize this
-- can ask if a trace satisisfies a formula

--Mission specifications
-- are used, among others, to synthesize, verify, simulate or guide
-- the engineering of robot software

--fair visit and patrolling, could be interpreted with respect to taking the cab around
-- a trace is just a path where the labeller is restricted to singleton sets

-- G (isRed -> (break U vel == 0) U isGreen) (unless there is an ambulance)

-- stop is really a slow and stay stop

-- G (isGreen -> Go)

-- globally, obey
\end{code}
