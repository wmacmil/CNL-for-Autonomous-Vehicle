{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Synonymize where

import Data.Maybe
import Control.Monad
import Drive
import LTL
import System.IO.Unsafe
import qualified PGF (Tree, showExpr)
import PGF hiding (Tree, showExpr)

{-
This file is intentended to do a quick semantic interpretation as a model and
example for a larger grammar being implemented. The limitations of this
degenerate grammar are simply to interpret sequences of unmodified actions as
systematically determined formulas in the fragment of linear temporal logic, one
can essentially just say to do things in a very deliberate sequence.

The GF grammar, outside of these abstract syntax functions outputs lists of
commands, ListCommands, whereby we only use the generic derived lists with at
least one Commands.

  OneCommand   : Command -> Commands ;
  CompoundCommand : [Command] -> Command ;
  SimpleCom      : Action   -> Command ;

The main thing that happens is, lists of commands in individual sentence are put
into a normal form, whereby all ambiguous parses are simply assumed to
correspond to a left-to-right sequence of commands. Then, the sentences are
included in the normal form.

normalizeList x
  where x = "go , stop and turn . stop , go and stop and turn . turn and go ."

yields
"go , stop , turn , stop , go , stop , turn , turn and go ."

The goal now is to take the AST of this normalized list and convert it into another haskell GADT which represents the LTL formula of some form like

AR : almost compositional function, via composOp used here
can think of this like a lens

idea : can canonically enforce a single AST for verification purposes given
two strings with unique ASTs

 need to basically create an equivalence class of synonyms, which aren't
 necessarily restricted to purely lexical items

-}

instance (Gf (Tree c)) => Show (Tree c) where
  show = PGF.showExpr [] . gf

gr :: IO PGF
gr = readPGF "pgf/Drive.pgf"

gr'   = unsafePerformIO $ readPGF "pgf/Drive.pgf"
cat   = startCat gr'
langs = languages gr'
eng   = head langs

semantics :: GListCommands -> Phi
semantics x =
  let (GListCommands ((GOneCommand y) : _)) = normalizeList x --hackey, assume single sentence
  in case y of
      q@(GSimpleCom a) -> astToAtom q
      (GCompoundCommand GAnd (GListPosCommand xs)) -> listCommand2LTL xs

-- turn left at the store, at the store is the outermost

listCommand2LTL :: [GPosCommand] -> Phi
listCommand2LTL [] = error "empty tree"
listCommand2LTL [GFinish] = G (Atom "FINISHED")
listCommand2LTL [q@(GSimpleCom x)] = F (astToAtom q)
listCommand2LTL (GSimpleCom (GModAction l (GWherePhrase left)):xs) =
  let
    x' = (astToAtom left)
    xs' = listCommand2LTL xs
    in X (Meet x' xs')
listCommand2LTL (q@(GSimpleCom x):xs) =
  let
    x'  = astToAtom' (getPossibleObj q)
    -- x'  = astToAtom (getPossibleObj q)
    xs' = listCommand2LTL xs
    in F (Meet x' xs')
listCommand2LTL (q@(GDoTil a x):xs) = -- need to customize for this
  let
    -- x'  = astToAtom (q)
    x'  = astToAtom' (getPossibleObj q)
    xs' = listCommand2LTL xs
    in F (Meet x' xs')

{-
-- whereCommand :: GPosCommand -> Bool
whereCommand :: GAction -> Bool
whereCommand (GModAction action phrase) =
  if hasWhere phrase then True else whereCommand action
whereCommand _ = False

-- hasWhere :: Tree a -> Bool
hasWhere :: GAdvPh -> Bool
hasWhere (GWherePhrase _) = True
hasWhere _ = False

-- maybe need to make everyhthing a continuation to be compatible with
-- so the question is if there is now a wherephrase, we have to notify our system
getPossibleWhere :: GAction -> Phi -> Phi
getPossibleWhere (GModAction l (GWherePhrase left)) = \x -> X (Meet (astToAtom left) x )
-}

-- Inari's wrapper solution
data SomeGf f = forall a. Gf (f a) => SomeGf (f a)

getPossibleObj :: GPosCommand -> SomeGf Tree
getPossibleObj (GSimpleCom (GModAction action (GMkAdvPh way obj))) = SomeGf obj
getPossibleObj (GDoTil action time) = SomeGf time
getPossibleObj x = SomeGf x

replace x1 x2 [] = []
replace x1 x2 (x:xs) | x == x1 = x2 : (replace x1 x2 xs)
replace x1 x2 (x:xs) | otherwise = x : (replace x1 x2 xs)

astToAtom' :: SomeGf Tree -> Phi
astToAtom' (SomeGf x) =
  let x' = gf x in
  Atom (replace ' ' '_' $ linearize gr' eng x')

-- astToAtom :: forall a. (Gf (Tree a)) => Tree a -> Phi
astToAtom x =
  let x' = gf x in
  Atom (replace ' ' '_' $ linearize gr' eng x')

-- Normalize the syntax tree--
-- ad hoc fix, how to do a better job on the base case (this wont generalize in the bigger case?)
-- Normalize the list to still fit a basic category
normalizeList ::  GListCommands -> GListCommands
normalizeList (GListCommands (GOneCommand (GSimpleCom x) : [])) = (GListCommands (GOneCommand (GSimpleCom x) : []))
normalizeList xs =
  let normalizedSublist = normalizeNestedLists xs
  in GListCommands ((GOneCommand (GCompoundCommand GAnd normalizedSublist)) : [])

normalizeNestedLists :: GListCommands -> GListPosCommand
normalizeNestedLists (GListCommands xs) = normalizeListPosCommand $ GListPosCommand (unSentence xs)

unSentence :: [GCommands] -> [GPosCommand]
unSentence ((GOneCommand x):xs) =  x : unSentence xs
unSentence _ = [] -- x ++ unSentence xs

-- Just working on in the context of an individual level.
-- Works by simply concatintating the lists with sublist cases
-- first recursively flattening to
normalizeListPosCommand :: GListPosCommand -> GListPosCommand
normalizeListPosCommand (GListPosCommand xs) = GListPosCommand (concatMap flattenSublist xs)
  where
    flattenSublist :: GPosCommand -> [GPosCommand]
    flattenSublist (GCompoundCommand c x') = (getListPosCommands (normalizeListPosCommand x'))
    flattenSublist x  = x : [] -- (GSimpleCom x')

getListPosCommands :: GListPosCommand -> [GPosCommand]
getListPosCommands (GListPosCommand x) = x

--STANDALONE PIPELINE--

-- lets us just view the modified tree
transformAST :: (GListCommands -> GListCommands) -> String -> String
transformAST f s = do
  let firstParse = head $ head $ parseAll gr' cat s
      internalRep = fg $ firstParse
      modifiedTree = f internalRep
      ast = gf modifiedTree
      in (linearize gr' eng ast)

-- applySem :: String -> Phi
applySem s = do
  let firstParse = head $ head $ parseAll gr' cat s
      internalRep = fg $ firstParse
      ltlAST = semantics internalRep
      in ltlAST

-- >>> transformAST femalePersonIsWoman goToTheWoman
-- example synonym transformation
femalePersonIsWoman :: forall a. Tree a -> Tree a
femalePersonIsWoman (GModObj GFemale GPerson) = GWoman
femalePersonIsWoman GWoman                    = (GModObj GFemale GPerson)
femalePersonIsWoman gp = composOp femalePersonIsWoman gp


directions = "turn left at the store , go to the store and turn left then turn right and turn right and go straight . go and turn left at the woman , turn right and stop . Finish ."
-- not entirely correct

-- >>> applySem directions
-- F (Meet (Atom "the_store") (F (Meet (Atom "the_store") (X (Meet (Atom "left") (X (Meet (Atom "right") (X (Meet (Atom "right") (X (Meet (Atom "straight") (F (Meet (Atom "go") (F (Meet (Atom "the_woman") (X (Meet (Atom "right") (F (Meet (Atom "stop") (G (Atom "FINISHED")))))))))))))))))))))

goToTheWoman = "go to the woman then go to the female person . stop at the woman ."
bigCafe = GWhichObject GThe (GModObj GBig GCafe)
goToTheBigCafe = (GSimpleCom (GModAction GGo (GMkAdvPh GTo bigCafe))) :: GPosCommand

-- >>> phi = head $ head $ parseAll gr' cat directions
-- >>> phi' = fg $ phi
-- >>> phi' :: GListCommands
-- >>> normalizeList phi'

-- >>> replace ' ' '_' "the man was cool"
-- "the_man_was_cool"

-- Big thing to notice : that different prepositions carry different semantic content, particulary in a logic meant to describe spatial and temporal relations

-- What do we want to verify? This then gives us into what we need to specify,
-- how to build the specification into our semantic space, and how to fit our
-- semantic space with the grammar. presumably, we to think of our semantic
-- space as like a typechecker, where we can filter out the possible meanings to
-- just one semantically, we might want to design the logic to turn, accelerate,
-- break (local vehicle controlers) are all next operations before gives an
-- interpretation of Not being in a state
-- turn needs to be grounded (i.e., if there is only one option, then we can only turn

-- would be really nice to have a partially quantified tree type, whereby one is only able to reach into subtrees (obviously certain types of clauses eliminate this)
-- turn left before the store and pull over to talk to the guy. then go back to the store.

-- maybe the easiest way is just to break it up at the AST level?
-- the more complicated we make it there, though, the less clean the grammar
-- ideally want a small grammar

-- [[go left at the store...]] = F (store /\ X (is_left ...))
-- [[go left after the store...]] = F (store /\ F (is_left ...))
-- [[go left before the store...]] = F (is_left /\ G (Neg store) ...))

-- go left soon after the store
-- go left before the store
