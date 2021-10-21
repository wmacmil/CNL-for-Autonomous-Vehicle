abstract Drive = {

flags startcat = Command ;

cat
  Command ;
  Place ;
  Time ;
  Route ;
  Action ; -- should this be dependently typed over controller
  Way ; -- in a certain way (adverb, or via some landmark)
  Direction ;
  Question ;
  Something ;
  Object ;
  -- Event ; -- dependent on time

  -- for places, times, orders : lists possibly ?

fun

  -- so a question is how composable are the actions

  Ask : Something -> Question ;

  SimpleCom : Action -> Command ;
  DriveTo : Action -> Way -> Place -> Command ;
  ModAction : Action -> Direction -> Action ; --adverbial
  -- EndRoute : Way -> Place -> Route ;

  DoTil : Action -> Time -> Command ;

  -- DriveTo : Action -> Route -> Command ; --go to the store
  -- EndRoute : Way -> Place -> Route ;
  -- MakeTurn : Action -> Direction -> Place -> Command ; --turn left after the tree

  Left   : Direction ;
  Right  : Direction ;
  Around : Direction ;

  At    : Way ;
  On    : Way ;
  In    : Way ;
  To    : Way ;
  From  : Way ;
  After : Way ;
  Under : Way ;
  Before : Way ;
  Over : Way ;

  Home : Place ;
  Cafe : Place ;
  Gallery : Place ;
  Museum : Place ;
  Bridge : Place ; -- also an object
  -- Overpass : Place ; -- also an object

  Edinburgh : Place ;
  London : Place ;
  Gothenburg : Place ;

  Go : Action ;
  Stop : Action ;
  Break : Action ;
  Turn : Action ;

  Tree : Object ;
  Car  : Object ;

  Now : Time ;
  InFive : Time ;

  Thing : Something ;
}
