
{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module GenerateAgda where

import qualified PGF (Tree, showExpr)
-- import PGF (Expr)
import PGF hiding (Tree, showExpr)
import PrettyPrint -- perhaps easier to do this manually
import Test

invertXY :: GX -> GY
invertXY Gx1 = Gy1
invertXY Gx2 = Gy2

--verify two flips indeed flips a sentence
flipSentence :: GS -> GS
flipSentence (GmkS1 x y) = (GmkS2 y x)
flipSentence (GmkS2 x y) = (GmkS1 y x)

data X where
  X1 :: X
  X2 :: X

data Y where
  Y1 :: Y
  Y2 :: Y

embedX :: GX -> X
embedX Gx1 = X1
embedX Gx2 = X2

-- >>> gr <- readPGF "Test.pgf"
-- >>> cat = startCat gr
-- >>> sum345 = "x1 y2"
-- >>> treeS345 = head $ head $ parseAll gr cat sum345
-- >>> treeS345
-- EApp (EApp (EFun mkS1) (EFun x1)) (EFun y2)
-- >>> eng = head $ languages gr
-- >>> foo = fg $ treeS345
-- >>> bar' = gf $ flipSentence foo
-- >>> linearize gr eng bar'
-- "y2 x1"

-- use pretty printer to linearize to Agda

-- linSent :: GX -> String
-- linSent x = "data " ++ _
