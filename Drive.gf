abstract Drive = {

flags startcat = Command ;

cat
  Command ;
  Place ;
  Time ;
  Route ;
  Action ; -- should this be dependently typed over controller
  Way ; -- in a certain way (adverb, or via some landmark), TODO: improve name
  Direction ;
  Thing ;
  Determ ;
  Object ;
  Number ;
  -- Event ; -- dependent on time

  -- GF supports lists
  [Command]{2} ;

  Question ; --QA, not relevant yet
  Something ;
  Conjunct ;

fun

  And : Conjunct ;
  Or : Conjunct ;
  Then : Conjunct ;

  SimpleCom : Action -> Command ;
  DriveTo : Action -> Way -> Place -> Command ;
  ModAction : Action -> Direction -> Action ;

  MultipleRoutes : Conjunct -> [Command] -> Command ;

  DoTil : Action -> Time -> Command ;

  WhichObject : Determ -> Thing -> Object ;
  ObjectPlace : Object -> Place ;

  MkNum : Int -> Number ;

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


  Cafe : Thing ;
  Gallery : Thing ;
  Museum : Thing ;
  Bridge : Thing ;

  Home : Place ;
  Edinburgh : Place ;
  London : Place ;
  Gothenburg : Place ;

  Drive : Action ;
  Go : Action ;
  Stop : Action ;
  Break : Action ;
  Turn : Action ;
  --Avoid, ....


  A : Determ ;
  The : Determ ;
  This : Determ ;
  These : Determ ;
  That : Determ ;

  Tree : Thing ;
  Car  : Thing ;
  Person : Thing ;

  Now : Time ;
  InFive : Time ;

  Ask : Something -> Question ; --just for illustrative purposes in the QA
  SomeThing : Something ;

}
