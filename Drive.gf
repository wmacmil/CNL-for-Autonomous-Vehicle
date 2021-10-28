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
  Direction ;
  UndetObj ;
  Determ ;
  Object ; --the cafe
  Number ;
  Conjunct ;
  Condition ;

  -- GF supports lists
  [Command]{2} ;
  [PosCommand]{2} ;
  [Place]{2} ;
  [Object]{2} ; --the cafe and the store

  --QA, not relevant yet
  Question ;
  Something ;

  -- Event ; -- dependent on time, need to figure out how to incorporate this

fun

  StartRoute : Keyword ;
  EndRoute : Keyword ;
  -- ModifyRoute : Keyword ;

  SetRoute : Keyword -> [Command] -> Keyword -> Route ;

  -- ControlledCom : Conjunct -> Condition -> PosCommand -> PosCommand ;

  -- MultipleCommands : 
  -- ModifyCommand : Conjunct -> Condition -> Command -> Command ;
  -- ModifyCommand : Condition -> Command -> Command ;
  ModifyCommand : Conjunct -> Condition -> PosCommand -> Command ; -- add polarity
  MKCommand : Polarity -> PosCommand -> Command ;

  SimpleCom : Action -> PosCommand ;
  DriveTo : Action -> Way -> Place -> PosCommand ;
  ModAction : Action -> Direction -> Action ;

  MultipleRoutes : Conjunct -> [PosCommand] -> PosCommand ;
  MultipleObject : Conjunct -> [Object] -> Object ; -- should we only coordinate places?
  MultiplePlaces : Conjunct -> [Place] -> Place ;

  DoTil : Action -> Time -> PosCommand ;

  WhichObject : Determ -> UndetObj -> Object ;
  ObjectPlace : Object -> Place ;

  UnlessSomething : UndetObj -> Condition ;


  MkNum : Int -> Determ ;

  And : Conjunct ;
  Unless : Conjunct ; -- need to figure out
  Then : Conjunct ; -- specifies temporal flow
  Or : Conjunct ; -- Requires QA, or a follow up at least

  -- "otherwise" should be like "if"
  -- mkUtt (mkAdv if_Subj (mkS (mkCl she_NP sleep_V)))

  Left   : Direction ;
  Right  : Direction ;
  Around : Direction ;

  At    : Way ;
  By    : Way ;
  On    : Way ;
  In    : Way ;
  To    : Way ;
  From  : Way ;
  After : Way ;
  Under : Way ;
  Before : Way ;
  Over : Way ;
  Until : Way ;
  Past : Way ;

  Cafe : UndetObj ;
  Store : UndetObj ;
  Gallery : UndetObj ;
  Museum : UndetObj ;
  Bridge : UndetObj ;
  Traffic : UndetObj ;

  Tree : UndetObj ;
  Car  : UndetObj ;
  Person : UndetObj ;
  --Obstacle, ...

  Home : Place ;
  Edinburgh : Place ;
  London : Place ;
  Gothenburg : Place ;

  Drive : Action ;
  Go : Action ;
  Stop : Action ;
  Break : Action ;
  Turn : Action ;
  --Avoid (the object), change/Shift (lanes), hang (a right), pull (a u-e), ....
  --keep (going, breaking, turning) ...

  A : Determ ;
  The : Determ ;
  This : Determ ;
  These : Determ ;
  That : Determ ;

  Now : Time ;
  InNMin : Determ -> Way -> Time ;

  --just for illustrative purposes in the QA
  Ask : Something -> Question ;
  SomeUndetObj : Something ;

}
