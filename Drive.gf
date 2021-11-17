abstract Drive = {

flags startcat = PosCommand ;

cat
  Keyword ; --Kinda meta-category, so that the user can unambiguously say something
  Route ;
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

  --QA, not relevant yet
  Question ;
  Something ;

  -- Event ; -- dependent on time, need to figure out how to incorporate this

fun


  -- ControlledCom : Conjunct -> Condition -> PosCommand -> PosCommand ;

  -- MultipleCommands :
  -- ModifyCommand : Conjunct -> Condition -> Command -> Command ;
  -- ModifyCommand : Condition -> Command -> Command ;
  ModifyCommand : Conjunct -> Condition -> PosCommand -> Command ; -- add polarity
  -- MKCommand : Polarity -> PosCommand -> Command ;

  -- idea : bury way, place inside an adverb
  SimpleCom : Action -> PosCommand ;
  -- DriveTo : Action -> Way -> Place -> PosCommand ;
  ModAction : Action -> AdvPh -> Action ;

  -- Direction : Direction -> How ;
  -- InWhatWay : Way -> Place -> How ;
  MkAdvPh : Way -> Object -> AdvPh ;
  HowPhrase : How -> AdvPh ;
  WherePhrase : Where -> AdvPh ;


  MultipleRoutes : Conjunct -> [PosCommand] -> PosCommand ;
  MultipleObject : Conjunct -> [Object] -> Object ; -- should we only coordinate places?
  MultiplePlaces : Conjunct -> [Place] -> Place ;

  DoTil : Action -> Time -> PosCommand ;



  WhichObject : Determ -> UndetObj -> Object ;
  ObjectPlace : Place -> Object ;

  UnlessSomething : UndetObj -> Condition ;

  --ignoring adjectival phrases
  ModObj : Descript -> UndetObj -> UndetObj ;
  -- ModObj : Descript -> UndetObj -> UndetObj ;

  PhraseModObj : UndetObj -> AdjPh -> UndetObj ;

  MkAdjPh : Way -> Object -> AdjPh ;

  -- AdjPhrase == ModObj

  MkNum : Int -> Determ ;

  InNMin : Determ -> Way -> Time ;

  Now : Time ;

  And    : Conjunct ;
  Unless : Conjunct ; -- need to figure out
  Then   : Conjunct ; -- specifies temporal flow
  Or     : Conjunct ; -- Requires QA, or a follow up at least

  -- grammatically similar, semantically distinct
  Quickly   : How ;
  Carefully : How ;
  -- in the left lane

  --previously "Direction"
  Left          : Where ;
  Right         : Where ;
  Around        : Where ;
  Straight : Where ;

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
