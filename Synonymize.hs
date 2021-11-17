{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Synonymize where

import qualified PGF (Tree, showExpr)
-- import PGF (Expr)
import PGF hiding (Tree, showExpr)
-- import PGF (Tree)
import Drive

-- idea : can canonically enforce a single AST for verification purposes given
-- two strings with unique ASTs
--
-- need to basically create an equivalence class of synonyms, which aren't
-- necessarily restricted to purely lexical items

-- Looking at the gf code
-- p -cat=UndetObj "female person"
-- ModObj Female Person

-- AR : almost compositional function
-- can think of this like a lens
transfer :: PGF.Tree -> PGF.Tree
transfer = gf . femalePersonIsWoman . fg

femalePersonIsWoman :: GUndetObj -> GUndetObj
-- femalePersonIsWoman (GModObj GFemale GPerson) = GWoman
femalePersonIsWoman GWoman                    = (GModObj GFemale GPerson)
femalePersonIsWoman x                         = x

-- Dante magic
-- >>> gr <- readPGF "Drive.pgf"
-- >>> cat = startCat gr
-- >>> goToTheWoman = "go to the woman"
-- >>> goToTheWomanT1 = head $ head $ parseAll gr cat goToTheWoman
-- >>> goToTheWomanT1
-- EApp (EFun SimpleCom) (EApp (EApp (EFun ModAction) (EFun Go)) (EApp (EApp (EFun MkAdvPh) (EFun To)) (EApp (EApp (EFun WhichObject) (EFun The)) (EFun Woman))))
-- >>> :t goToTheWomanT1
-- goToTheWomanT1 :: Tree
-- >>> eng = head $ languages gr
-- >>> goToTheWomanAST = fg $ goToTheWomanT1
-- >>> goToTheWomanAST :: GPosCommand
-- GSimpleCom (GModAction GGo (GMkAdvPh GTo (GWhichObject GThe GWoman)))
-- >>> goToTheFemalePersonT1 = transfer goToTheWomanT1
-- >>> :t goToTheFemalePersonT1
-- goToTheFemalePersonT1 :: Tree
-- >>> linearize gr eng goToTheFemalePersonT1
-- "*** Exception: no UndetObj EApp (EFun SimpleCom) (EApp (EApp (EFun ModAction) (EFun Go)) (EApp (EApp (EFun MkAdvPh) (EFun To)) (EApp (EApp (EFun WhichObject) (EFun The)) (EFun Woman))))
-- CallStack (from HasCallStack):
--   error, called at ./Drive.hs:481:12 in main:Drive

-- "*** Exception: no UndetObj EApp (EFun SimpleCom) (EApp (EApp (EFun ModAction) (EFun Go)) (EApp (EApp (EFun MkAdvPh) (EFun To)) (EApp (EApp (EFun WhichObject) (EFun The)) (EFun Woman))))
-- CallStack (from HasCallStack):
--   error, called at ./Drive.hs:481:12 in main:Drive
