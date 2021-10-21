concrete DriveEng of Drive = open
    SyntaxEng,
    SymbolicEng,
    (C = ConstructorsEng),
    ParadigmsEng,
    -- ExtraEng, -- for the negative prop
    Prelude in {

lincat
  Command = Imp ;
  Place = NP ;
  Route = Adv ;
  Action = VP ;
  Way = Prep ; -- in a certain way (adverb, or via some landmark)
  Direction = Adv ;
  Question = QS ;
  Something = QCl ;
  Object = NP ;
  Time = Adv ;
  -- Event ;

lin

  -- Ask : Something -> Question ;
  Ask s = mkQS s ;

lin

  -- SimpleCom : Action -> Command ;
  SimpleCom a = mkImp a ;
  -- DriveTo : Action -> Way -> Place -> Command ;
  DriveTo a w p = mkImp (mkVP a (C.mkAdv w p)) ;


  -- MultipleRoutes : Command -> Command -> Command ;


  -- then , adverb

  -- ModAction : Action -> Direction -> Action ;
  ModAction a d = mkVP a d ;

  DoTil a t = mkImp (mkVP a t) ; --  "turn right in five minutes"

  -- TODO
  -- turn right five minutes after the cafe
  -- (more carefully) turn right in five minutes once you've passed the cafe (yuck)
  -- the cafe becomes an event, and five minutes after

  -- -- EndRoute : Way -> Place -> Route ;
  -- EndRoute w p = mkAdv w p ;

oper
  five    = mkDet (mkCard (mkNumeral n5_Unit)) ;
  minutes = mkN "minute" "minutes" ;
  fiveminutes = mkNP five minutes ;
  then = mkConj "then" ; --how to compose imperatives
  --go to the cafe then the bakery
  --go to the cafe then turn right
  -- problem is, Imp is very high and therefore it requires a custom use of the conjuction then


lin

  Now = ParadigmsEng.mkAdv "now" ;
  InFive = C.mkAdv in_Prep fiveminutes ;

  -- (furure) event is a time and thing at the time

  -- mkUtt (mkCard at_least_AdN (mkCard (mkNumeral n8_Unit)))
  -- InFive = mkAdv in_Prep (mkNP  Time ; -- generalize to mkTime

  -- mkUtt (mkImp (mkVP (mkVP come_V) (mkAdv to_Prep (mkNP i_Pron house_N))))
  -- later
  -- after some event
  -- DriveTo : Action -> Way -> Place -> Route ;
  -- TakeAction : gcAction -> Action ;


  --  : Direction ;
  Left = ParadigmsEng.mkAdv "left" ;
  Right = ParadigmsEng.mkAdv "right" ;
  Around = ParadigmsEng.mkAdv "around" ;

  -- way
  At = mkPrep "at" ;
  On = on_Prep ;
  In = in_Prep ;
  To = to_Prep ;
  From  = from_Prep ;
  After = after_Prep ;
  Under = under_Prep ;
  Over  = mkPrep "over" ;
  Before  = mkPrep "before" ;


  -- place
  Cafe = mkNP the_Det (mkN "cafe") ;
  Gallery = mkNP the_Det (mkN "gallery") ;
  Museum = mkNP the_Det (mkN "museum") ;
  Bridge = mkNP the_Det (mkN "bridge") ; -- also an object

  Home = mkNP (mkN "home") ;
  Edinburgh = mkNP (mkPN "Edinburgh") ;
  London = mkNP (mkPN "London") ;
  Gothenburg = mkNP (mkPN "Gothenburg") ;

  -- action
  Drive = mkVP (mkV "drive") ;
  Go = mkVP (mkV "go") ;
  Stop = mkVP (mkV "stop") ;
  Break = mkVP (mkV "break") ;
  Turn = mkVP (mkV "turn") ;

  -- object
  Tree = mkNP the_Det (mkN "tree") ;
  Car = mkNP the_Det (mkN "car") ;

  -- Thing : Something ;
  Thing = mkQCl who_IP (mkV "something") ;

}
