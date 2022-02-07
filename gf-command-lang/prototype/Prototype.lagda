\begin{code}
module Prototype where

open import Data.Nat -- with natural ordering, may encode temporality
open import Data.Unit
open import Data.Empty
open import Data.List
open import Data.Sum
open import Data.Product
\end{code}

Prior Critique :

The decidability of type-checking in type theories like those which underly
Agda, while from some perspectives can be seen as (with valid arguments for the
postion) a crutch, it rules out programs like the following,
"RiemannHypothesisOrItsNegation → IO ()" which prints out the proof of the
Riemann Hypothesis. For, to the construtivist, this program won't typecheck
unless one has either proven or disproven the Reimann hypothesis, and to create
a type-checking algorithm capable of proving the Riemann Hypothesis would be a
research feat unimaginable to mortals.

When designing a machine learning interface capable of doing taking advanced
natural language commands, like "Go right if theres a red light", one wants to
similarly rule out pathological examples such as "Go left if the Riemann
hypothesis is true". A human driver would not even understand the command unless
they are mathematically fluent, and even if they were, they would be incapable
of deciding wether or not to turn. When trying to extend coverage of a natural
language component, it is up to the designer of the system where to draw the
line. This may be by choice when making up a Controlled Natural Language (CNL),
or possibly subject to the conditions of the data some algorithm is trained on.

Regardless, this a priori decision may and likely will have deep implications
for what the system is capable of and how usable it is. We begin with a
"minimalalist menatiality" here, acknowledging that this may be brittle will not
get very far when it comes to an actual system which may eventually be
incorporated into a real autonomous robot. To the machine learning enthusiast,
the lack of appeal will be in the fact that nothing is learned, rather, it is
specified. Nonetheless, the fact that it is specified and inductively defined,
as we do below, gives us a much better way of reasoning about our system, and
verifying its correctness. The point at which it becomes overlyspecified, and
the transition to a purely learned language, should be made trying to balance
practical and theoretical concerns, subject to the availability of emerical data
and the possibility of actual real-world usage.

We name these ideas in anticipation of objections, but more importantly, to
guide our own design and iterative processes.

-----

TODO :
- rename stuff (both grammatical and semantic categories as currently
- make more compositional
- align/compare with GF grammar

Conventions :
- Data types : capitalized
- Constructors : lower case
- camelCase throughout

We note that the main concern of our system is of a user giving the vehicle (or
perhaps better thought of as a robot more generally) natural langauge commands,
e.g. utterances in the *imperative form*. The extension to a question answer (QA)
system, would be ideal, but this would not explicity linked with the behavior of
the robot as regards how the user controls its motion, our main concern here. It
is the job for some other person, although perhaps we should pretend that there
is a prepended command to signal to the vehicle which control system should be
targeted.

Additionally, we are starting with an incredibly small lexicon, with a sparse
set of combinators over the lexemes. In a programming language design,
minimilism is *generally* to be preferred, in part because

(i) The more primities to learn, the more difficult the learning curve for the programmer
(ii) The bigger the language, the more difficult it is to apply meta-reasoning principles to
(iii) The right set of combinators should generally be expressive enough to encode more complex ideas

This is obviously subject to much debate among the PL community. When designing
a Controlled Natural Language (CNL), a Domain Specific Language (DSL) with with
a natural language syntax, both (i) and (ii) still apply. However, (iii) is
quite different. One does not define natural language inductively, and
therefore, one cannot simply define new lexical items nor new grammatical rules
out of old ones. Although statistical, or "machine learning" methods may give us
a way of expanding our lexicon and well as cover much more grammatical ground,
doing so negates (i) and (ii). Reconciling these differences is an important and
difficult question.

(Our approach to synonyms with respect to new lexical items out of old ones does
kind of exploit this approach, but it's purely with regards to our application)

We begin, in some sense, with the "base ontology", the things the objects our
system has to navigate. These are all comon nouns. One could instead choose
animal instead of dog, where "beingDog" is a property of an animal for instance,
but this level of refinement may come in later. In some cases, dog or animal
may well be treated as synonymous, and in other cases not.

\begin{code}
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
  school  : BaseObject
  traffic : BaseObject

\end{code}

Next, focus on ways of modifying nouns, namely adjectives. We treat this as a
notion of "compound object"; however, some auxiliary notions must be defined
prior, not just adjectives.

What may function as an adjective grammatically, however, doesn't just consist
of subset of the dictionary labelled adjective, because the in the phrase "woman
with the dog", "with the dog" modifies woman like an adjective. We therefore
define the primitive set of prepositions now, even though some of them will only
be used to create phrases used to modify adverbs later on.

We append a tick to the prepostions already resereved for Agda.

\begin{code}

-- way in GF file
data Prep : Set where
  before : Prep
  after  : Prep
  on     : Prep
  to     : Prep
  from   : Prep
  under  : Prep
  over   : Prep
  until  : Prep
  past   : Prep
  in'    : Prep -- agda keywords
  with'  : Prep --

\end{code}

We additionally include propor nouns, which, in English are undetermined and
capitilized (we break with the constructor convention). Martin Luther King
Boulevard, is a common street name in the United States, and I include it even
though I doubt there are many streets named as such in Europe.

\begin{code}

data Place : Set where

  Edinburgh  : Place
  London     : Place
  Gothenburg : Place
  NewYork    : Place
  MLKblvd    : Place

\end{code}

We include the basic determiners here, which are incredibly interesting from a
semantic perspective (beginning with Montague), but also obviously occupy a
large role in the type theory community as well.

\begin{code}

data Determ : Set where

  a     : Determ
  the   : Determ
  this  : Determ
  these : Determ
  that  : Determ
  those : Determ
  numdet : ℕ → Determ

\end{code}

Conjunctions can be used to coordinate all types of parts of speech
linguistically, but we introduce them here because they can obviously be used to
coordinate nouns, both determined and undetermined.

Please note :

- "Then" is an adverb used as a coordinating conjunction
- "before/after" are prepositions but treated as conjunctions semantically, sometimes
- once one uses conjuncts, everything needs to be listified
- can actual give these semantically as a logical conjunctive form if one desires a deep agda embedding
- There can be spatial and temporal coordination, oftentimes using the same conjuction (give exampeles)
- Or generally, requires clarification from vehicle
  + howevever, man or tree could presume the man is at the tree

\begin{code}

data Conjunct : Set where
  and'   : Conjunct
  unless : Conjunct
  then'  : Conjunct
  or'    : Conjunct

\end{code}

To define our notion of compound object, including those with adjectival
phrases, we realize that a determined object, which itself may include a
modified object, (think "the cute puppy"), may also be used to modify another
object and thus make it compopund, like "girl with the cute puppy". We therefore
require a mutual inductive definition.

Originally we had, due to Matthew's suggestion, a coproduct arguement in the
comp constructor for compound objects, in order to accomodate other languages,
like german.

-- comp : CompoundObj → List (ADJ ⊎ CompoundObj) → CompoundObj

However, we will witness this as either :

(i) a grammatical artificact that shouldn't effect the way we reason
semantically (or at least in this context)

(ii) subsumed in some adjectival phrase

Comments on this from people with other opinions, and knowledge of other
languages, are welcome.

-- I think adjectival phrases is linguistically incorrect

\begin{code}
mutual -- so that adjective phrases can reference nouns

  -- quality from GF
  data ADJ : Set where
    female    : ADJ
    male      : ADJ
    big       : ADJ
    small     : ADJ
    fast      : ADJ
    slow      : ADJ
    living    : ADJ
    nonliving : ADJ
    number    : ℕ → ADJ -- two streets
    adjPhrase : Prep → DetObj → ADJ -- with the dog


  data CompoundObj : Set where
    basecomp : BaseObject → CompoundObj
    comp     : CompoundObj → List ADJ → CompoundObj
    conjObj  : Conjunct → CompoundObj → CompoundObj

  data DetObj : Set where
    Home    : DetObj -- tricky because this doesn't require a determiner even. ad-hoc here
    MkPlace : Place → DetObj
    NP      : Determ → CompoundObj → DetObj
    ConjDetObj : Determ → DetObj → DetObj → DetObj

\end{code}

Finally, we come to verbs, which we call actions. These are modified similairly
to nouns, with the (unrealistic but practical) exception that there is no way of
of simply modifying a verb with another verb. If one does take into account an
adverbial clause, however, it is presumed we will have to make most of the file
a mutual inductive defition.

Again, going to our motiviting critique, we want to rule out what we'll call
"Forest Gump sentences" like "drive to the man sitting on the bench, talking to
the woman next to him about his childhood in rural Alabama, which is about ...."

The way adverbs and adjectives modify is different, "big red ball" versus "turn
slowly and gently", although most likely these details are irrelevant for a first pass.

\begin{code}

data BaseAction : Set where
  turn   : BaseAction
  break' : BaseAction
  go     : BaseAction
  drive  : BaseAction
  honk   : BaseAction

data ADV : Set where
  left      : ADV
  right     : ADV
  around    : ADV
  conAdv    : Conjunct → ADV → ADV → ADV -- go quickly but cautiously through this neigborhood
  advPhrase : Prep → DetObj → ADV

data CompoundAction : Set where
  basecompV : BaseAction → CompoundAction
  compV : CompoundAction → List ADV → CompoundAction

\end{code}

We finally showcase the notion of a command, a single compound action or
multiple action with various ways of coordinating them.


\begin{code}

data Command : Set where
  baseCommand : CompoundAction → Command
  consCommand : Conjunct → CompoundAction → Command → Command

\end{code}

We now showcase examples utterances using this lexicon and "grammar" below.

\begin{code}

-- TEST LEXICON
-- coercion for synonym
road = basecomp street

woman : CompoundObj
woman = comp (basecomp human) (female ∷ [])

man : CompoundObj
man = comp (basecomp human) (male ∷ [])

-- do we have to do these dummy coercions everywhere?
dog' : CompoundObj
dog' = basecomp dog

withTheDog : ADJ
withTheDog = adjPhrase with' (NP the dog')

manWithTheDog : CompoundObj
manWithTheDog = comp (basecomp human) (male ∷ withTheDog ∷ [])

justGo : Command
justGo = baseCommand (basecompV go)

turnLeftAfterTheMan : Command
turnLeftAfterTheMan =
  baseCommand (compV (basecompV turn) (left ∷ (advPhrase after (NP the man)) ∷ []))

turnLeftAfterTwoStreets : Command
turnLeftAfterTwoStreets  =
  baseCommand (compV (basecompV turn) (left ∷ (advPhrase after (NP (numdet 2) road)) ∷ []))

turnLeftIn3Minutes : Command
turnLeftIn3Minutes =
  baseCommand (compV (basecompV turn) (left ∷ (advPhrase after (NP (numdet 3) (basecomp minute))) ∷ []))

-- until we hit the school
-- untilWeHitTheSchool
goFastUntilTheSchool =
  (compV (basecompV go) ((advPhrase until (NP the (basecomp school))) ∷ []))

turnLeftAfterTheManWithTheDog =
  (compV (basecompV turn) (left ∷ (advPhrase after (NP the manWithTheDog)) ∷ []))

turnLeftAfterTheManWithTheDog' : Command
turnLeftAfterTheManWithTheDog' =
  baseCommand (compV (basecompV turn) (left ∷ (advPhrase after (NP the manWithTheDog)) ∷ []))

turnLeftAfterTheManWithTheDogThenGoFastUntilTheSchool : Command
turnLeftAfterTheManWithTheDogThenGoFastUntilTheSchool =
  consCommand then' turnLeftAfterTheManWithTheDog  (baseCommand goFastUntilTheSchool)

turnLeftThenTurnRight : Command
turnLeftThenTurnRight =
  consCommand then'
    turnLeft
    (baseCommand turnRight)
  where
    turnLeft = (compV (basecompV turn) (left ∷ []))
    turnRight = (compV (basecompV turn) (right ∷ []))

\end{code}

Posthumous Ideas: 

-- if this is actually generated from a data set (where the users are somehow
-- confined to the cnl, then pathological examples (grammatical, semantic) should be ruled out
-- using gf allows us to filter at least the grammatical examples
-- how to transform the GF code to agda code

--responsibility is presumed by the system
-- go fast and responsibly
-- go reasonably fast
-- go reasonably -- not



-- turn right == turn to the right ?
-- turn gently to the left
-- turn gently to the left

-- turn left before you turn right == turn right after you turn left
-- manWithTheWoman == womanWithTheMan
-- manWithTheWoman == womanWithTheMan
-- manAndWoman == womanAndMan , commutivity of conjunction

-- What kind of logical structure on top of this?

-- CompleteCommand = List Command

-- "turn left after the man"
-- Idea CompoundObj + CompoundAction

