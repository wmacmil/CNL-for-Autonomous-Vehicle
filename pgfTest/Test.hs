module Test where

import PGF hiding (Tree)
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

newtype GString = GString String deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double deriving Show

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GS =
   GmkS1 GX GY 
 | GmkS2 GY GX 
  deriving Show

data GX =
   Gx1 
 | Gx2 
  deriving Show

data GY =
   Gy1 
 | Gy2 
  deriving Show


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


