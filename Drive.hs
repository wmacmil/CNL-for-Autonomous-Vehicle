module Drive where

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

data GAction =
   GBreak 
 | GDrive 
 | GGo 
 | GModAction GAction GAdvPh 
 | GStop 
 | GTurn 
  deriving Show

data GAdjPh = GMkAdjPh GWay GObject 
  deriving Show

data GAdvPh =
   GHowPhrase GHow 
 | GMkAdvPh GWay GObject 
 | GWherePhrase GWhere 
  deriving Show

data GCommand = GModifyCommand GConjunct GCondition GPosCommand 
  deriving Show

data GCondition = GUnlessSomething GUndetObj 
  deriving Show

data GConjunct =
   GAnd 
 | GOr 
 | GThen 
 | GUnless 
  deriving Show

data GDescript =
   GBig 
 | GFast 
 | GFemale 
 | GLiving 
 | GMale 
 | GNonliving 
 | GSlow 
 | GSmall 
  deriving Show

data GDeterm =
   GA 
 | GMkNum GInt 
 | GThat 
 | GThe 
 | GThese 
 | GThis 
  deriving Show

data GHow =
   GCarefully 
 | GQuickly 
  deriving Show

data GKeyword =
   GEndRoute 
 | GStartRoute 
  deriving Show

newtype GListObject = GListObject [GObject] deriving Show

newtype GListPlace = GListPlace [GPlace] deriving Show

newtype GListPosCommand = GListPosCommand [GPosCommand] deriving Show

data GObject =
   GMultipleObject GConjunct GListObject 
 | GObjectPlace GPlace 
 | GWhichObject GDeterm GUndetObj 
  deriving Show

data GPlace =
   GEdinburgh 
 | GGothenburg 
 | GHome 
 | GLondon 
 | GMultiplePlaces GConjunct GListPlace 
  deriving Show

data GPosCommand =
   GDoTil GAction GTime 
 | GMultipleRoutes GConjunct GListPosCommand 
 | GSimpleCom GAction 
  deriving Show

data GQuestion = GAsk GSomething 
  deriving Show

data GSomething = GSomeUndetObj 
  deriving Show

data GTime =
   GInNMin GDeterm GWay 
 | GNow 
  deriving Show

data GUndetObj =
   GBridge 
 | GCafe 
 | GCar 
 | GDog 
 | GGallery 
 | GModObj GDescript GUndetObj 
 | GMuseum 
 | GPerson 
 | GPhraseModObj GUndetObj GAdjPh 
 | GStore 
 | GTraffic 
 | GTree 
 | GWoman 
  deriving Show

data GWay =
   GAfter 
 | GAt 
 | GBefore 
 | GBy 
 | GFrom 
 | GIn 
 | GOn 
 | GOver 
 | GPast 
 | GTo 
 | GUnder 
 | GUntil 
 | GWith 
  deriving Show

data GWhere =
   GAround 
 | GLeft 
 | GRight 
 | GStraight 
  deriving Show

data GNumber

data GPolarity

data GRoute


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




