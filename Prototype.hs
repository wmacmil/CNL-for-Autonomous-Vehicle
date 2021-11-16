{-# LANGUAGE GADTs                 #-}

module Prototype where

data BaseObject where
  Human   :: BaseObject
  Dog     :: BaseObject
  Car     :: BaseObject
  Tree    :: BaseObject
  Street  :: BaseObject
  Minute  :: BaseObject
  Cafe    :: BaseObject
  Store   :: BaseObject
  Gallery :: BaseObject
  Museum  :: BaseObject
  Bridge  :: BaseObject
  School  :: BaseObject
  Traffic :: BaseObject

data Prep where
  Before :: Prep
  After  :: Prep
  On     :: Prep
  To     :: Prep
  From   :: Prep
  Under  :: Prep
  Over   :: Prep
  Until  :: Prep
  Past   :: Prep
  In'    :: Prep -- agda keywords
  With'  :: Prep --

data Place where
  Edinburgh  :: Place
  London     :: Place
  Gothenburg :: Place
  NewYork    :: Place
  MLKblvd    :: Place

data Determ where

  A     :: Determ
  The   :: Determ
  This  :: Determ

data Conjunct where
  And'   :: Conjunct
  Unless :: Conjunct
  Then'  :: Conjunct
  Or'    :: Conjunct

-- mutual -- so that adjective phrases can reference nouns

  -- quality from GF
data ADJ where
  Female    :: ADJ
  Male      :: ADJ
  Big       :: ADJ
  Small     :: ADJ
  Fast      :: ADJ
  Slow      :: ADJ
  Living    :: ADJ
  Nonliving :: ADJ
  -- NumberAdj    :: ℕ → ADJ -- two streets
  AdjPhrase :: Prep -> DetObj -> ADJ -- with the dog


data CompoundObj where
  Basecomp :: BaseObject -> CompoundObj
  Comp     :: CompoundObj -> [ADJ] -> CompoundObj
  ConjObj  :: Conjunct -> CompoundObj -> CompoundObj

data DetObj where
  Home    :: DetObj
  MkPlace :: Place -> DetObj
  NP      :: Determ -> CompoundObj -> DetObj
  ConjDetObj :: Determ -> DetObj -> DetObj -> DetObj

data BaseAction where
  Turn   :: BaseAction
  Break' :: BaseAction
  Go     :: BaseAction
  Drive  :: BaseAction
  Honk   :: BaseAction

data ADV where
  Left'      :: ADV
  Right'     :: ADV
  Around    :: ADV
  ConAdv    :: Conjunct -> ADV -> ADV -> ADV -- go quickly but cautiously through this neigborhood
  AdvPhrase :: Prep -> DetObj -> ADV

data CompoundAction where
  BasecompV :: BaseAction -> CompoundAction
  CompV     :: CompoundAction -> [ADV] -> CompoundAction


road = Basecomp Street

woman :: CompoundObj
woman = Comp (Basecomp Human) (Female : []) 

dog :: CompoundObj
dog = Basecomp Dog

withTheDog :: ADJ
withTheDog = AdjPhrase With' (NP The dog)

manWithTheDog :: CompoundObj
manWithTheDog = Comp (Basecomp Human) (Male : withTheDog : [])

turnLeftAfterTheManWithTheDog =
  (CompV (BasecompV Turn) (Left' : (AdvPhrase After (NP The manWithTheDog)) : []))
