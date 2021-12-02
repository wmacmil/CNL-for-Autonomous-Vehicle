# CNL-for-Autonomous-Vehicle

Please Note : this a work in progress and contributions are welcome.

Directories : 
* report : latex summary of work done and todos. please reference .pdf
* code  : 
  + GF grammar
  + PGF embedding
  + Different Haskell representation of grammar
  + Agda representation to mirror the Haskell one above
* text-files : notes, ideas, hypothetical command lexicon, etc

Note : GF allows for many morphological details to be brushed under the rug
(the semantics, singular or plural, of the determiner determins, the
inflection of the noun tree" - the same AST node Tree yields two different
linearizations.

  "stop after these trees"
  DriveTo Stop After (ObjectPlace (WhichObject These Tree))

  p "stop after this tree"
  DriveTo Stop After (ObjectPlace (WhichObject This Tree))

"These" vs "those" have a spatial component, and u

