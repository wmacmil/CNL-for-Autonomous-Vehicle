{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module Drive where

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

type GAction = Tree GAction_
data GAction_
type GAdjPh = Tree GAdjPh_
data GAdjPh_
type GAdvPh = Tree GAdvPh_
data GAdvPh_
type GCommand = Tree GCommand_
data GCommand_
type GCondition = Tree GCondition_
data GCondition_
type GConjunct = Tree GConjunct_
data GConjunct_
type GDescript = Tree GDescript_
data GDescript_
type GDeterm = Tree GDeterm_
data GDeterm_
type GHow = Tree GHow_
data GHow_
type GKeyword = Tree GKeyword_
data GKeyword_
type GListObject = Tree GListObject_
data GListObject_
type GListPlace = Tree GListPlace_
data GListPlace_
type GListPosCommand = Tree GListPosCommand_
data GListPosCommand_
type GObject = Tree GObject_
data GObject_
type GPlace = Tree GPlace_
data GPlace_
type GPosCommand = Tree GPosCommand_
data GPosCommand_
type GQuestion = Tree GQuestion_
data GQuestion_
type GSomething = Tree GSomething_
data GSomething_
type GTime = Tree GTime_
data GTime_
type GUndetObj = Tree GUndetObj_
data GUndetObj_
type GWay = Tree GWay_
data GWay_
type GWhere = Tree GWhere_
data GWhere_
type GNumber = Tree GNumber_
data GNumber_
type GPolarity = Tree GPolarity_
data GPolarity_
type GRoute = Tree GRoute_
data GRoute_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  GBreak :: Tree GAction_
  GDrive :: Tree GAction_
  GGo :: Tree GAction_
  GModAction :: GAction -> GAdvPh -> Tree GAction_
  GStop :: Tree GAction_
  GTurn :: Tree GAction_
  GMkAdjPh :: GWay -> GObject -> Tree GAdjPh_
  GHowPhrase :: GHow -> Tree GAdvPh_
  GMkAdvPh :: GWay -> GObject -> Tree GAdvPh_
  GWherePhrase :: GWhere -> Tree GAdvPh_
  GModifyCommand :: GConjunct -> GCondition -> GPosCommand -> Tree GCommand_
  GUnlessSomething :: GUndetObj -> Tree GCondition_
  GAnd :: Tree GConjunct_
  GOr :: Tree GConjunct_
  GThen :: Tree GConjunct_
  GUnless :: Tree GConjunct_
  GBig :: Tree GDescript_
  GFast :: Tree GDescript_
  GFemale :: Tree GDescript_
  GLiving :: Tree GDescript_
  GMale :: Tree GDescript_
  GNonliving :: Tree GDescript_
  GSlow :: Tree GDescript_
  GSmall :: Tree GDescript_
  GA :: Tree GDeterm_
  GMkNum :: GInt -> Tree GDeterm_
  GThat :: Tree GDeterm_
  GThe :: Tree GDeterm_
  GThese :: Tree GDeterm_
  GThis :: Tree GDeterm_
  GCarefully :: Tree GHow_
  GQuickly :: Tree GHow_
  GEndRoute :: Tree GKeyword_
  GStartRoute :: Tree GKeyword_
  GListObject :: [GObject] -> Tree GListObject_
  GListPlace :: [GPlace] -> Tree GListPlace_
  GListPosCommand :: [GPosCommand] -> Tree GListPosCommand_
  GMultipleObject :: GConjunct -> GListObject -> Tree GObject_
  GObjectPlace :: GPlace -> Tree GObject_
  GWhichObject :: GDeterm -> GUndetObj -> Tree GObject_
  GEdinburgh :: Tree GPlace_
  GGothenburg :: Tree GPlace_
  GHome :: Tree GPlace_
  GLondon :: Tree GPlace_
  GMultiplePlaces :: GConjunct -> GListPlace -> Tree GPlace_
  GDoTil :: GAction -> GTime -> Tree GPosCommand_
  GMultipleRoutes :: GConjunct -> GListPosCommand -> Tree GPosCommand_
  GSimpleCom :: GAction -> Tree GPosCommand_
  GAsk :: GSomething -> Tree GQuestion_
  GSomeUndetObj :: Tree GSomething_
  GInNMin :: GDeterm -> GWay -> Tree GTime_
  GNow :: Tree GTime_
  GBridge :: Tree GUndetObj_
  GCafe :: Tree GUndetObj_
  GCar :: Tree GUndetObj_
  GDog :: Tree GUndetObj_
  GGallery :: Tree GUndetObj_
  GModObj :: GDescript -> GUndetObj -> Tree GUndetObj_
  GMuseum :: Tree GUndetObj_
  GPerson :: Tree GUndetObj_
  GPhraseModObj :: GUndetObj -> GAdjPh -> Tree GUndetObj_
  GStore :: Tree GUndetObj_
  GTraffic :: Tree GUndetObj_
  GTree :: Tree GUndetObj_
  GWoman :: Tree GUndetObj_
  GAfter :: Tree GWay_
  GAt :: Tree GWay_
  GBefore :: Tree GWay_
  GBy :: Tree GWay_
  GFrom :: Tree GWay_
  GIn :: Tree GWay_
  GOn :: Tree GWay_
  GOver :: Tree GWay_
  GPast :: Tree GWay_
  GTo :: Tree GWay_
  GUnder :: Tree GWay_
  GUntil :: Tree GWay_
  GWith :: Tree GWay_
  GAround :: Tree GWhere_
  GLeft :: Tree GWhere_
  GRight :: Tree GWhere_
  GStraight :: Tree GWhere_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GBreak,GBreak) -> and [ ]
    (GDrive,GDrive) -> and [ ]
    (GGo,GGo) -> and [ ]
    (GModAction x1 x2,GModAction y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GStop,GStop) -> and [ ]
    (GTurn,GTurn) -> and [ ]
    (GMkAdjPh x1 x2,GMkAdjPh y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GHowPhrase x1,GHowPhrase y1) -> and [ x1 == y1 ]
    (GMkAdvPh x1 x2,GMkAdvPh y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GWherePhrase x1,GWherePhrase y1) -> and [ x1 == y1 ]
    (GModifyCommand x1 x2 x3,GModifyCommand y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GUnlessSomething x1,GUnlessSomething y1) -> and [ x1 == y1 ]
    (GAnd,GAnd) -> and [ ]
    (GOr,GOr) -> and [ ]
    (GThen,GThen) -> and [ ]
    (GUnless,GUnless) -> and [ ]
    (GBig,GBig) -> and [ ]
    (GFast,GFast) -> and [ ]
    (GFemale,GFemale) -> and [ ]
    (GLiving,GLiving) -> and [ ]
    (GMale,GMale) -> and [ ]
    (GNonliving,GNonliving) -> and [ ]
    (GSlow,GSlow) -> and [ ]
    (GSmall,GSmall) -> and [ ]
    (GA,GA) -> and [ ]
    (GMkNum x1,GMkNum y1) -> and [ x1 == y1 ]
    (GThat,GThat) -> and [ ]
    (GThe,GThe) -> and [ ]
    (GThese,GThese) -> and [ ]
    (GThis,GThis) -> and [ ]
    (GCarefully,GCarefully) -> and [ ]
    (GQuickly,GQuickly) -> and [ ]
    (GEndRoute,GEndRoute) -> and [ ]
    (GStartRoute,GStartRoute) -> and [ ]
    (GListObject x1,GListObject y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListPlace x1,GListPlace y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListPosCommand x1,GListPosCommand y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GMultipleObject x1 x2,GMultipleObject y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GObjectPlace x1,GObjectPlace y1) -> and [ x1 == y1 ]
    (GWhichObject x1 x2,GWhichObject y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GEdinburgh,GEdinburgh) -> and [ ]
    (GGothenburg,GGothenburg) -> and [ ]
    (GHome,GHome) -> and [ ]
    (GLondon,GLondon) -> and [ ]
    (GMultiplePlaces x1 x2,GMultiplePlaces y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GDoTil x1 x2,GDoTil y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GMultipleRoutes x1 x2,GMultipleRoutes y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GSimpleCom x1,GSimpleCom y1) -> and [ x1 == y1 ]
    (GAsk x1,GAsk y1) -> and [ x1 == y1 ]
    (GSomeUndetObj,GSomeUndetObj) -> and [ ]
    (GInNMin x1 x2,GInNMin y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNow,GNow) -> and [ ]
    (GBridge,GBridge) -> and [ ]
    (GCafe,GCafe) -> and [ ]
    (GCar,GCar) -> and [ ]
    (GDog,GDog) -> and [ ]
    (GGallery,GGallery) -> and [ ]
    (GModObj x1 x2,GModObj y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GMuseum,GMuseum) -> and [ ]
    (GPerson,GPerson) -> and [ ]
    (GPhraseModObj x1 x2,GPhraseModObj y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GStore,GStore) -> and [ ]
    (GTraffic,GTraffic) -> and [ ]
    (GTree,GTree) -> and [ ]
    (GWoman,GWoman) -> and [ ]
    (GAfter,GAfter) -> and [ ]
    (GAt,GAt) -> and [ ]
    (GBefore,GBefore) -> and [ ]
    (GBy,GBy) -> and [ ]
    (GFrom,GFrom) -> and [ ]
    (GIn,GIn) -> and [ ]
    (GOn,GOn) -> and [ ]
    (GOver,GOver) -> and [ ]
    (GPast,GPast) -> and [ ]
    (GTo,GTo) -> and [ ]
    (GUnder,GUnder) -> and [ ]
    (GUntil,GUntil) -> and [ ]
    (GWith,GWith) -> and [ ]
    (GAround,GAround) -> and [ ]
    (GLeft,GLeft) -> and [ ]
    (GRight,GRight) -> and [ ]
    (GStraight,GStraight) -> and [ ]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GAction where
  gf GBreak = mkApp (mkCId "Break") []
  gf GDrive = mkApp (mkCId "Drive") []
  gf GGo = mkApp (mkCId "Go") []
  gf (GModAction x1 x2) = mkApp (mkCId "ModAction") [gf x1, gf x2]
  gf GStop = mkApp (mkCId "Stop") []
  gf GTurn = mkApp (mkCId "Turn") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Break" -> GBreak 
      Just (i,[]) | i == mkCId "Drive" -> GDrive 
      Just (i,[]) | i == mkCId "Go" -> GGo 
      Just (i,[x1,x2]) | i == mkCId "ModAction" -> GModAction (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Stop" -> GStop 
      Just (i,[]) | i == mkCId "Turn" -> GTurn 


      _ -> error ("no Action " ++ show t)

instance Gf GAdjPh where
  gf (GMkAdjPh x1 x2) = mkApp (mkCId "MkAdjPh") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "MkAdjPh" -> GMkAdjPh (fg x1) (fg x2)


      _ -> error ("no AdjPh " ++ show t)

instance Gf GAdvPh where
  gf (GHowPhrase x1) = mkApp (mkCId "HowPhrase") [gf x1]
  gf (GMkAdvPh x1 x2) = mkApp (mkCId "MkAdvPh") [gf x1, gf x2]
  gf (GWherePhrase x1) = mkApp (mkCId "WherePhrase") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "HowPhrase" -> GHowPhrase (fg x1)
      Just (i,[x1,x2]) | i == mkCId "MkAdvPh" -> GMkAdvPh (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "WherePhrase" -> GWherePhrase (fg x1)


      _ -> error ("no AdvPh " ++ show t)

instance Gf GCommand where
  gf (GModifyCommand x1 x2 x3) = mkApp (mkCId "ModifyCommand") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "ModifyCommand" -> GModifyCommand (fg x1) (fg x2) (fg x3)


      _ -> error ("no Command " ++ show t)

instance Gf GCondition where
  gf (GUnlessSomething x1) = mkApp (mkCId "UnlessSomething") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "UnlessSomething" -> GUnlessSomething (fg x1)


      _ -> error ("no Condition " ++ show t)

instance Gf GConjunct where
  gf GAnd = mkApp (mkCId "And") []
  gf GOr = mkApp (mkCId "Or") []
  gf GThen = mkApp (mkCId "Then") []
  gf GUnless = mkApp (mkCId "Unless") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "And" -> GAnd 
      Just (i,[]) | i == mkCId "Or" -> GOr 
      Just (i,[]) | i == mkCId "Then" -> GThen 
      Just (i,[]) | i == mkCId "Unless" -> GUnless 


      _ -> error ("no Conjunct " ++ show t)

instance Gf GDescript where
  gf GBig = mkApp (mkCId "Big") []
  gf GFast = mkApp (mkCId "Fast") []
  gf GFemale = mkApp (mkCId "Female") []
  gf GLiving = mkApp (mkCId "Living") []
  gf GMale = mkApp (mkCId "Male") []
  gf GNonliving = mkApp (mkCId "Nonliving") []
  gf GSlow = mkApp (mkCId "Slow") []
  gf GSmall = mkApp (mkCId "Small") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Big" -> GBig 
      Just (i,[]) | i == mkCId "Fast" -> GFast 
      Just (i,[]) | i == mkCId "Female" -> GFemale 
      Just (i,[]) | i == mkCId "Living" -> GLiving 
      Just (i,[]) | i == mkCId "Male" -> GMale 
      Just (i,[]) | i == mkCId "Nonliving" -> GNonliving 
      Just (i,[]) | i == mkCId "Slow" -> GSlow 
      Just (i,[]) | i == mkCId "Small" -> GSmall 


      _ -> error ("no Descript " ++ show t)

instance Gf GDeterm where
  gf GA = mkApp (mkCId "A") []
  gf (GMkNum x1) = mkApp (mkCId "MkNum") [gf x1]
  gf GThat = mkApp (mkCId "That") []
  gf GThe = mkApp (mkCId "The") []
  gf GThese = mkApp (mkCId "These") []
  gf GThis = mkApp (mkCId "This") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "A" -> GA 
      Just (i,[x1]) | i == mkCId "MkNum" -> GMkNum (fg x1)
      Just (i,[]) | i == mkCId "That" -> GThat 
      Just (i,[]) | i == mkCId "The" -> GThe 
      Just (i,[]) | i == mkCId "These" -> GThese 
      Just (i,[]) | i == mkCId "This" -> GThis 


      _ -> error ("no Determ " ++ show t)

instance Gf GHow where
  gf GCarefully = mkApp (mkCId "Carefully") []
  gf GQuickly = mkApp (mkCId "Quickly") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Carefully" -> GCarefully 
      Just (i,[]) | i == mkCId "Quickly" -> GQuickly 


      _ -> error ("no How " ++ show t)

instance Gf GKeyword where
  gf GEndRoute = mkApp (mkCId "EndRoute") []
  gf GStartRoute = mkApp (mkCId "StartRoute") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "EndRoute" -> GEndRoute 
      Just (i,[]) | i == mkCId "StartRoute" -> GStartRoute 


      _ -> error ("no Keyword " ++ show t)

instance Gf GListObject where
  gf (GListObject [x1,x2]) = mkApp (mkCId "BaseObject") [gf x1, gf x2]
  gf (GListObject (x:xs)) = mkApp (mkCId "ConsObject") [gf x, gf (GListObject xs)]
  fg t =
    GListObject (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseObject" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsObject" -> fg x1 : fgs x2


      _ -> error ("no ListObject " ++ show t)

instance Gf GListPlace where
  gf (GListPlace [x1,x2]) = mkApp (mkCId "BasePlace") [gf x1, gf x2]
  gf (GListPlace (x:xs)) = mkApp (mkCId "ConsPlace") [gf x, gf (GListPlace xs)]
  fg t =
    GListPlace (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BasePlace" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsPlace" -> fg x1 : fgs x2


      _ -> error ("no ListPlace " ++ show t)

instance Gf GListPosCommand where
  gf (GListPosCommand [x1,x2]) = mkApp (mkCId "BasePosCommand") [gf x1, gf x2]
  gf (GListPosCommand (x:xs)) = mkApp (mkCId "ConsPosCommand") [gf x, gf (GListPosCommand xs)]
  fg t =
    GListPosCommand (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BasePosCommand" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsPosCommand" -> fg x1 : fgs x2


      _ -> error ("no ListPosCommand " ++ show t)

instance Gf GObject where
  gf (GMultipleObject x1 x2) = mkApp (mkCId "MultipleObject") [gf x1, gf x2]
  gf (GObjectPlace x1) = mkApp (mkCId "ObjectPlace") [gf x1]
  gf (GWhichObject x1 x2) = mkApp (mkCId "WhichObject") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "MultipleObject" -> GMultipleObject (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ObjectPlace" -> GObjectPlace (fg x1)
      Just (i,[x1,x2]) | i == mkCId "WhichObject" -> GWhichObject (fg x1) (fg x2)


      _ -> error ("no Object " ++ show t)

instance Gf GPlace where
  gf GEdinburgh = mkApp (mkCId "Edinburgh") []
  gf GGothenburg = mkApp (mkCId "Gothenburg") []
  gf GHome = mkApp (mkCId "Home") []
  gf GLondon = mkApp (mkCId "London") []
  gf (GMultiplePlaces x1 x2) = mkApp (mkCId "MultiplePlaces") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Edinburgh" -> GEdinburgh 
      Just (i,[]) | i == mkCId "Gothenburg" -> GGothenburg 
      Just (i,[]) | i == mkCId "Home" -> GHome 
      Just (i,[]) | i == mkCId "London" -> GLondon 
      Just (i,[x1,x2]) | i == mkCId "MultiplePlaces" -> GMultiplePlaces (fg x1) (fg x2)


      _ -> error ("no Place " ++ show t)

instance Gf GPosCommand where
  gf (GDoTil x1 x2) = mkApp (mkCId "DoTil") [gf x1, gf x2]
  gf (GMultipleRoutes x1 x2) = mkApp (mkCId "MultipleRoutes") [gf x1, gf x2]
  gf (GSimpleCom x1) = mkApp (mkCId "SimpleCom") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "DoTil" -> GDoTil (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "MultipleRoutes" -> GMultipleRoutes (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "SimpleCom" -> GSimpleCom (fg x1)


      _ -> error ("no PosCommand " ++ show t)

instance Gf GQuestion where
  gf (GAsk x1) = mkApp (mkCId "Ask") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "Ask" -> GAsk (fg x1)


      _ -> error ("no Question " ++ show t)

instance Gf GSomething where
  gf GSomeUndetObj = mkApp (mkCId "SomeUndetObj") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "SomeUndetObj" -> GSomeUndetObj 


      _ -> error ("no Something " ++ show t)

instance Gf GTime where
  gf (GInNMin x1 x2) = mkApp (mkCId "InNMin") [gf x1, gf x2]
  gf GNow = mkApp (mkCId "Now") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "InNMin" -> GInNMin (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Now" -> GNow 


      _ -> error ("no Time " ++ show t)

instance Gf GUndetObj where
  gf GBridge = mkApp (mkCId "Bridge") []
  gf GCafe = mkApp (mkCId "Cafe") []
  gf GCar = mkApp (mkCId "Car") []
  gf GDog = mkApp (mkCId "Dog") []
  gf GGallery = mkApp (mkCId "Gallery") []
  gf (GModObj x1 x2) = mkApp (mkCId "ModObj") [gf x1, gf x2]
  gf GMuseum = mkApp (mkCId "Museum") []
  gf GPerson = mkApp (mkCId "Person") []
  gf (GPhraseModObj x1 x2) = mkApp (mkCId "PhraseModObj") [gf x1, gf x2]
  gf GStore = mkApp (mkCId "Store") []
  gf GTraffic = mkApp (mkCId "Traffic") []
  gf GTree = mkApp (mkCId "Tree") []
  gf GWoman = mkApp (mkCId "Woman") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Bridge" -> GBridge 
      Just (i,[]) | i == mkCId "Cafe" -> GCafe 
      Just (i,[]) | i == mkCId "Car" -> GCar 
      Just (i,[]) | i == mkCId "Dog" -> GDog 
      Just (i,[]) | i == mkCId "Gallery" -> GGallery 
      Just (i,[x1,x2]) | i == mkCId "ModObj" -> GModObj (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Museum" -> GMuseum 
      Just (i,[]) | i == mkCId "Person" -> GPerson 
      Just (i,[x1,x2]) | i == mkCId "PhraseModObj" -> GPhraseModObj (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Store" -> GStore 
      Just (i,[]) | i == mkCId "Traffic" -> GTraffic 
      Just (i,[]) | i == mkCId "Tree" -> GTree 
      Just (i,[]) | i == mkCId "Woman" -> GWoman 


      _ -> error ("no UndetObj " ++ show t)

instance Gf GWay where
  gf GAfter = mkApp (mkCId "After") []
  gf GAt = mkApp (mkCId "At") []
  gf GBefore = mkApp (mkCId "Before") []
  gf GBy = mkApp (mkCId "By") []
  gf GFrom = mkApp (mkCId "From") []
  gf GIn = mkApp (mkCId "In") []
  gf GOn = mkApp (mkCId "On") []
  gf GOver = mkApp (mkCId "Over") []
  gf GPast = mkApp (mkCId "Past") []
  gf GTo = mkApp (mkCId "To") []
  gf GUnder = mkApp (mkCId "Under") []
  gf GUntil = mkApp (mkCId "Until") []
  gf GWith = mkApp (mkCId "With") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "After" -> GAfter 
      Just (i,[]) | i == mkCId "At" -> GAt 
      Just (i,[]) | i == mkCId "Before" -> GBefore 
      Just (i,[]) | i == mkCId "By" -> GBy 
      Just (i,[]) | i == mkCId "From" -> GFrom 
      Just (i,[]) | i == mkCId "In" -> GIn 
      Just (i,[]) | i == mkCId "On" -> GOn 
      Just (i,[]) | i == mkCId "Over" -> GOver 
      Just (i,[]) | i == mkCId "Past" -> GPast 
      Just (i,[]) | i == mkCId "To" -> GTo 
      Just (i,[]) | i == mkCId "Under" -> GUnder 
      Just (i,[]) | i == mkCId "Until" -> GUntil 
      Just (i,[]) | i == mkCId "With" -> GWith 


      _ -> error ("no Way " ++ show t)

instance Gf GWhere where
  gf GAround = mkApp (mkCId "Around") []
  gf GLeft = mkApp (mkCId "Left") []
  gf GRight = mkApp (mkCId "Right") []
  gf GStraight = mkApp (mkCId "Straight") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Around" -> GAround 
      Just (i,[]) | i == mkCId "Left" -> GLeft 
      Just (i,[]) | i == mkCId "Right" -> GRight 
      Just (i,[]) | i == mkCId "Straight" -> GStraight 


      _ -> error ("no Where " ++ show t)

instance Show GNumber

instance Gf GNumber where
  gf _ = undefined
  fg _ = undefined



instance Show GPolarity

instance Gf GPolarity where
  gf _ = undefined
  fg _ = undefined



instance Show GRoute

instance Gf GRoute where
  gf _ = undefined
  fg _ = undefined




instance Compos Tree where
  compos r a f t = case t of
    GModAction x1 x2 -> r GModAction `a` f x1 `a` f x2
    GMkAdjPh x1 x2 -> r GMkAdjPh `a` f x1 `a` f x2
    GHowPhrase x1 -> r GHowPhrase `a` f x1
    GMkAdvPh x1 x2 -> r GMkAdvPh `a` f x1 `a` f x2
    GWherePhrase x1 -> r GWherePhrase `a` f x1
    GModifyCommand x1 x2 x3 -> r GModifyCommand `a` f x1 `a` f x2 `a` f x3
    GUnlessSomething x1 -> r GUnlessSomething `a` f x1
    GMkNum x1 -> r GMkNum `a` f x1
    GMultipleObject x1 x2 -> r GMultipleObject `a` f x1 `a` f x2
    GObjectPlace x1 -> r GObjectPlace `a` f x1
    GWhichObject x1 x2 -> r GWhichObject `a` f x1 `a` f x2
    GMultiplePlaces x1 x2 -> r GMultiplePlaces `a` f x1 `a` f x2
    GDoTil x1 x2 -> r GDoTil `a` f x1 `a` f x2
    GMultipleRoutes x1 x2 -> r GMultipleRoutes `a` f x1 `a` f x2
    GSimpleCom x1 -> r GSimpleCom `a` f x1
    GAsk x1 -> r GAsk `a` f x1
    GInNMin x1 x2 -> r GInNMin `a` f x1 `a` f x2
    GModObj x1 x2 -> r GModObj `a` f x1 `a` f x2
    GPhraseModObj x1 x2 -> r GPhraseModObj `a` f x1 `a` f x2
    GListObject x1 -> r GListObject `a` foldr (a . a (r (:)) . f) (r []) x1
    GListPlace x1 -> r GListPlace `a` foldr (a . a (r (:)) . f) (r []) x1
    GListPosCommand x1 -> r GListPosCommand `a` foldr (a . a (r (:)) . f) (r []) x1
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
