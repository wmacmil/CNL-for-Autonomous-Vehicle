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

listCommand2LTL :: [GPosCommand] -> Phi
listCommand2LTL [] = error "empty tree"
listCommand2LTL [GFinish] = G (Atom "FINISHED")
listCommand2LTL [q@(GSimpleCom x)] = F (astToAtom q)
listCommand2LTL (q@(GSimpleCom x):xs) =
  let
    x'  = astToAtom (q)
    xs' = listCommand2LTL xs
    in F (Meet x' xs')
listCommand2LTL (q@(GDoTil a x):xs) =
  let
    x'  = astToAtom (q)
    xs' = listCommand2LTL xs
    in F (Meet x' xs')


  -- ModAction : Action -> AdvPh -> Action ;
  -- MkAdvPh     : Way   -> Object -> AdvPh ;
  -- HowPhrase   : How   -> AdvPh ;
  -- WherePhrase : Where -> AdvPh ;

-- turn :: GAction -> Phi
-- turn (GModAction (GLeft) l) = X _
-- turn (GModAction l (GWherePhrase wh)) = X _
-- -- turn (GModAction GRight l) = _
-- -- turn :: (GSimpleCom (GModAction action (GMkAdvPh way obj)))

-- (GSimpleCom (GModAction action (GMkAdvPh way obj))) = obj
-- getAction :: forall a . (Gf (Tree a)) => GPosCommand -> Tree a

-- -- getPossibleObj :: GPosCommand -> GObject
-- getPossibleObj :: (Gf a) => GPosCommand -> a
-- getPossibleObj (GSimpleCom (GModAction action (GMkAdvPh way obj))) = obj
-- getPossibleObj x = x
-- -- getAction (GSimpleCom _) = _



replace x1 x2 [] = []
replace x1 x2 (x:xs) | x == x1 = x2 : (replace x1 x2 xs)
replace x1 x2 (x:xs) | otherwise = x : (replace x1 x2 xs)

-- >>> :t astToAtom
-- astToAtom :: Gf a => a -> Phi

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
-- "go to the female person then go to the woman . stop at the female person ."
-- example synonym transformation
femalePersonIsWoman :: forall a. Tree a -> Tree a
femalePersonIsWoman (GModObj GFemale GPerson) = GWoman
femalePersonIsWoman GWoman                    = (GModObj GFemale GPerson)
femalePersonIsWoman gp = composOp femalePersonIsWoman gp

directions = "turn left in 5 minutes , go to the store and turn left then turn right and turn right and go straight . go and turn left at the woman , turn right and stop . Finish ."
goToTheWoman = "go to the woman then go to the female person . stop at the woman ."
bigCafe = GWhichObject GThe (GModObj GBig GCafe)
goToTheBigCafe = (GSimpleCom (GModAction GGo (GMkAdvPh GTo bigCafe))) :: GPosCommand

-- >>> applySem directions -- <interactive>:605:11-18: error:
-- F (Meet (Atom "turn_left_in_5_minutes") (F (Meet (Atom "go_to_the_store") (F (Meet (Atom "turn_left") (F (Meet (Atom "turn_right") (F (Meet (Atom "turn_right") (F (Meet (Atom "go_straight") (F (Meet (Atom "go") (F (Meet (Atom "turn_left_at_the_woman") (F (Meet (Atom "turn_right") (F (Meet (Atom "stop") (G (Atom "FINISHED")))))))))))))))))))))

-- >>> phi = head $ head $ parseAll gr' cat directions
-- >>> phi' = fg $ phi
-- >>> phi' :: GListCommands
-- ConsCommands (OneCommand (CompoundCommand And (ConsPosCommand (DoTil (ModAction Turn (WherePhrase Left)) (InNMin (MkNum 5) In)) (BasePosCommand (SimpleCom (ModAction Go (MkAdvPh To (WhichObject The Store)))) (CompoundCommand Then (BasePosCommand (SimpleCom (ModAction Turn (WherePhrase Left))) (CompoundCommand And (BasePosCommand (SimpleCom (ModAction Turn (WherePhrase Right))) (CompoundCommand And (BasePosCommand (SimpleCom (ModAction Turn (WherePhrase Right))) (SimpleCom (ModAction Go (WherePhrase Straight))))))))))))) (ConsCommands (OneCommand (CompoundCommand And (BasePosCommand (SimpleCom Go) (CompoundCommand And (ConsPosCommand (SimpleCom (ModAction (ModAction Turn (WherePhrase Left)) (MkAdvPh At (WhichObject The Woman)))) (BasePosCommand (SimpleCom (ModAction Turn (WherePhrase Right))) (SimpleCom Stop))))))) (BaseCommands (OneCommand Finish)))
-- >>> normalizeList phi'
-- BaseCommands (OneCommand (CompoundCommand And (ConsPosCommand (DoTil (ModAction Turn (WherePhrase Left)) (InNMin (MkNum 5) In)) (ConsPosCommand (SimpleCom (ModAction Go (MkAdvPh To (WhichObject The Store)))) (ConsPosCommand (SimpleCom (ModAction Turn (WherePhrase Left))) (ConsPosCommand (SimpleCom (ModAction Turn (WherePhrase Right))) (ConsPosCommand (SimpleCom (ModAction Turn (WherePhrase Right))) (ConsPosCommand (SimpleCom (ModAction Go (WherePhrase Straight))) (ConsPosCommand (SimpleCom Go) (ConsPosCommand (SimpleCom (ModAction (ModAction Turn (WherePhrase Left)) (MkAdvPh At (WhichObject The Woman)))) (ConsPosCommand (SimpleCom (ModAction Turn (WherePhrase Right))) (BasePosCommand (SimpleCom Stop) Finish))))))))))))


-- >>> replace ' ' '_' "the man was cool"
-- "the_man_was_cool"
