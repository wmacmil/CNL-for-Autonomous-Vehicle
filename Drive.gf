abstract Drive = {

flags startcat = PosCommand ;

cat
  Command ;
  Polarity ;
  PosCommand ;
  Place ;
  Time ;
  Action ; -- should this be dependently typed over controller
  Way ; -- in a certain way (adverb, or via some landmark), TODO: improve name
  -- Direction ;
  How ;
  Where ;
  AdvPh ; -- TODO: better name for adverbial phrase, maybe coproducts in ast
  AdjPh ;
  UndetObj ;
  Determ ;
  Object ; --the cafe
  Number ;
  Conjunct ;
  Condition ;
  Descript ; --adjective

  -- GF supports lists
  -- [Command]{2} ;
  [PosCommand]{2} ;
  [Place]{2} ;
  [Object]{2} ; --the cafe and the store

  -- TODO
  Route ;
  Keyword ; --Kinda meta-category, so that the user can unambiguously say something
  --QA, not relevant yet
  Question ;
  Something ;

  -- Event ; -- dependent on time, need to figure out how to incorporate this

fun

  -- ControlledCom : Conjunct -> Condition -> PosCommand -> PosCommand ;

  -- MKCommand : Polarity -> PosCommand -> Command ;
  ModifyCommand : Conjunct -> Condition -> PosCommand -> Command ; -- add polarity

  UnlessSomething : UndetObj -> Condition ;

  MultipleRoutes : Conjunct -> [PosCommand] -> PosCommand ;
  DoTil          : Action   -> Time         -> PosCommand ;
  SimpleCom      : Action   -> PosCommand ;

  ModAction : Action -> AdvPh -> Action ;

  MkAdvPh     : Way   -> Object -> AdvPh ;
  HowPhrase   : How   -> AdvPh ;
  WherePhrase : Where -> AdvPh ;

  MultipleObject : Conjunct -> [Object] -> Object ; -- should we only coordinate places?
  WhichObject    : Determ   -> UndetObj -> Object ;
  ObjectPlace    : Place    -> Object ;

  MultiplePlaces : Conjunct -> [Place] -> Place ;

  ModObj       : Descript -> UndetObj -> UndetObj ;
  PhraseModObj : UndetObj -> AdjPh    -> UndetObj ;

  MkAdjPh : Way -> Object -> AdjPh ;

  InNMin : Determ -> Way -> Time ;

  MkNum : Int -> Determ ;

  Now : Time ;

  And    : Conjunct ;
  Unless : Conjunct ; -- need to figure out
  Then   : Conjunct ; -- specifies temporal flow
  Or     : Conjunct ; -- Requires QA, or a follow up at least

  -- grammatically similar, semantically distinct
  Quickly   : How ;
  Carefully : How ;
  -- in the left lane

  Left          : Where ;
  Right         : Where ;
  Around        : Where ;
  Straight      : Where ;

  At     : Way ;
  With   : Way ;
  By     : Way ;
  On     : Way ;
  In     : Way ;
  To     : Way ;
  From   : Way ;
  After  : Way ;
  Under  : Way ;
  Before : Way ;
  Over   : Way ;
  Until  : Way ;
  Past   : Way ;

  Cafe    : UndetObj ;
  Store   : UndetObj ;
  Gallery : UndetObj ;
  Museum  : UndetObj ;
  Bridge  : UndetObj ;
  Traffic : UndetObj ;

  Tree   : UndetObj ;
  Car    : UndetObj ;
  Person : UndetObj ;
  Dog    : UndetObj ;
  --Obstacle, ...

  Home       : Place ;
  Edinburgh  : Place ;
  London     : Place ;
  Gothenburg : Place ;
  -- MLKblvd : Place ;

  Drive : Action ;
  Go    : Action ;
  Stop  : Action ;
  Break : Action ;
  Turn  : Action ;

  Female    : Descript ;
  Male      : Descript ;
  Big       : Descript ;
  Small     : Descript ;
  Fast      : Descript ;
  Slow      : Descript ;
  Living    : Descript ;
  Nonliving : Descript ;

  A     : Determ ;
  The   : Determ ;
  This  : Determ ;
  These : Determ ;
  That  : Determ ;


  -- BEGIN META Stuff, Ignore for now
  StartRoute : Keyword ;
  EndRoute : Keyword ;
  -- ModifyRoute : Keyword ;
  -- SetRoute : Keyword -> [Command] -> Keyword -> Route ;

  --just for illustrative purposes in the QA
  Ask : Something -> Question ;
  SomeUndetObj : Something ;
  -- END META

  -- GENERAL THOUGHTS, should be cleaned up soon

  -- still want a semantic distinction between adverbial vs adjectival
  -- preposotional phrases

  -- overgenerative parsing is a feature of semantic degeneracy

  -- p "drive to the male person with the dog after the tree"
  -- lots of parses


  -- "otherwise" should be like "if"
  -- mkUtt (mkAdv if_Subj (mkS (mkCl she_NP sleep_V)))

  -- to the right --
  -- to the store
  -- general adverb modifier, could be broken up depending on the semantic anaylsis

  --Avoid (the object), change/Shift (lanes), hang (a right), pull (a u-e), ....
  --keep (going, breaking, turning) ...

}
