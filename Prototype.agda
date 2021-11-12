module Prototype where

open import Data.Nat -- with natural ordering, may encode temporality
open import Data.Unit
open import Data.Empty
open import Data.List
open import Data.Sum
open import Data.Product

-- TODO :
--   rename stuff (both grammatical and semantic categories as currently
--   make more compositional

-- this is in some sense the "base ontology"
data BaseObject : Set where

  human   : BaseObject
  dog     : BaseObject
  car     : BaseObject
  tree    : BaseObject
  street  : BaseObject
  minute  : BaseObject
  cafe    : BaseObject
  store   : BaseObject
  gallery : BaseObject
  museum  : BaseObject
  bridge  : BaseObject
  traffic : BaseObject

--way
data Prep : Set where
  after  : Prep
  before : Prep
  on     : Prep
  to     : Prep
  from   : Prep
  under  : Prep
  over   : Prep
  until  : Prep
  past   : Prep
  in'    : Prep -- agda keywords
  with'  : Prep --

data Place : Set where
  Edinburgh  : Place
  London     : Place
  Gothenburg : Place

--after is a preposition but treated as a conjunction semantically
-- then is an adverb used as a coordinating conjunction
-- once one uses conjuncts, everything needs to be listified
-- can actual give these a logical conjunctive form if one desires an agda embedding
data Conjunct : Set where
  and'   : Conjunct
  unless : Conjunct
  then'  : Conjunct
  or'    : Conjunct -- requires clarification from vehicle

mutual -- so that adjective phrases can reference nouns

  -- quality
  data ADJ : Set where
    female    : ADJ
    male      : ADJ
    big       : ADJ
    small     : ADJ
    fast      : ADJ
    slow      : ADJ
    living    : ADJ
    nonliving : ADJ
    number    : ℕ → ADJ
    adjPhrase : Prep → DetObj → ADJ -- with the dog

  data CompoundObj : Set where
    basecomp : BaseObject → CompoundObj
    comp     : CompoundObj → List (ADJ ⊎ CompoundObj) → CompoundObj
    -- ⊎ hypothetically used for other languages

  data Determ : Set where
    a     : Determ
    the   : Determ
    this  : Determ
    these : Determ
    that  : Determ
    those : Determ

  data DetObj : Set where
    Home    : DetObj -- tricky because this doesn't require a determiner even
    MkPlace : Place → DetObj
    NP      : Determ → CompoundObj → DetObj

data BaseAction : Set where
  turn   : BaseAction
  break' : BaseAction
  go     : BaseAction
  drive  : BaseAction
  honk   : BaseAction

data ADV : Set where
  left : ADV
  right : ADV
  around : ADV
  advPhrase : Prep → DetObj → ADV

data CompoundAction : Set where
  basecompV : BaseAction → CompoundAction
  compV : CompoundAction → List (ADV ⊎ CompoundAction) → CompoundAction

-- Command : Set
-- Command = CompoundAction

data Command : Set where
  baseCommand : CompoundAction → Command
  consCommand : Conjunct → CompoundAction → Command → Command

-- coercion for synonym
road = basecomp street

woman : CompoundObj
woman = comp (basecomp human) (inj₁ female ∷ [])

man : CompoundObj
man = comp (basecomp human) (inj₁ male ∷ [])

-- do we have to do these dummy coercions everywhere?
dog' : CompoundObj
dog' = basecomp dog

withTheDog : ADJ
withTheDog = adjPhrase with' (NP the dog')

manWithTheDog : CompoundObj
manWithTheDog = comp (basecomp human) (inj₁ male ∷ inj₁ withTheDog ∷ [])

turnLeftAfterTheMan : Command
turnLeftAfterTheMan =
  baseCommand (compV (basecompV turn) (inj₁ left ∷ (inj₁ (advPhrase after (NP the man)) ∷ [])))

-- turnLeftAfterTheMan : Command
-- turnLeftAfterTheMan =
--   baseCommand (compV (basecompV turn) (inj₁ left ∷ (inj₁ (advPhrase after (NP the man)) ∷ [])))

turnLeftAfterTheManWithTheDog =
  (compV (basecompV turn) (inj₁ left ∷ (inj₁ (advPhrase after (NP the manWithTheDog)) ∷ [])))

turnLeftAfterTheManWithTheDog' : Command
turnLeftAfterTheManWithTheDog' =
  baseCommand (compV (basecompV turn) (inj₁ left ∷ (inj₁ (advPhrase after (NP the manWithTheDog)) ∷ [])))

justGo : Command
justGo = baseCommand (basecompV go)

turnLeft = (compV (basecompV turn) (inj₁ left ∷ []))
-- turnLeft = (compV (basecompV turn) (inj₁ left ∷ []))

turnLeftThenTurnRight : Command
turnLeftThenTurnRight =
  consCommand then'
    (compV (basecompV turn) (inj₁ left ∷ []))
    (baseCommand (compV (basecompV turn) (inj₁ right ∷ [])))


-- if the riemann hypothesis is true, turn left

-- manWithTheWoman == womanWithTheMan
-- manWithTheWoman == womanWithTheMan

-- What kind of logical structure on top of this?

-- CompleteCommand = List Command

-- "turn left after the man"
-- Idea CompoundObj + CompoundAction
