{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module Test where

import Control.Monad.Identity
import Data.Monoid
import PGF hiding (Tree)
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

type GS = Tree GS_
data GS_
type GX = Tree GX_
data GX_
type GY = Tree GY_
data GY_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  GmkS1 :: GX -> GY -> Tree GS_
  GmkS2 :: GY -> GX -> Tree GS_
  Gx1 :: Tree GX_
  Gx2 :: Tree GX_
  Gy1 :: Tree GY_
  Gy2 :: Tree GY_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GmkS1 x1 x2,GmkS1 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GmkS2 x1 x2,GmkS2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (Gx1,Gx1) -> and [ ]
    (Gx2,Gx2) -> and [ ]
    (Gy1,Gy1) -> and [ ]
    (Gy2,Gy2) -> and [ ]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GS where
  gf (GmkS1 x1 x2) = mkApp (mkCId "mkS1") [gf x1, gf x2]
  gf (GmkS2 x1 x2) = mkApp (mkCId "mkS2") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "mkS1" -> GmkS1 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "mkS2" -> GmkS2 (fg x1) (fg x2)


      _ -> error ("no S " ++ show t)

instance Gf GX where
  gf Gx1 = mkApp (mkCId "x1") []
  gf Gx2 = mkApp (mkCId "x2") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "x1" -> Gx1 
      Just (i,[]) | i == mkCId "x2" -> Gx2 


      _ -> error ("no X " ++ show t)

instance Gf GY where
  gf Gy1 = mkApp (mkCId "y1") []
  gf Gy2 = mkApp (mkCId "y2") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "y1" -> Gy1 
      Just (i,[]) | i == mkCId "y2" -> Gy2 


      _ -> error ("no Y " ++ show t)


instance Compos Tree where
  compos r a f t = case t of
    GmkS1 x1 x2 -> r GmkS1 `a` f x1 `a` f x2
    GmkS2 x1 x2 -> r GmkS2 `a` f x1 `a` f x2
    _ -> r t

class Compos t where
  compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
         -> (forall a. t a -> m (t a)) -> t c -> m (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . composOpM (Identity . f)

composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composOpM = compos return ap

composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m
composOpMonoid = composOpFold mempty mappend

composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b
composOpMPlus = composOpFold mzero mplus

composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

newtype C b a = C { unC :: b }
