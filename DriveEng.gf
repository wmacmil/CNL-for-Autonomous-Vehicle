concrete DriveEng of Drive = open
    SyntaxEng,
    SymbolicEng,
    (C = ConstructorsEng),
    ParadigmsEng,
    -- ExtraEng, -- for the negative prop
    ExtendEng, -- for the negative prop
    Prelude in {

lincat
  Command = Imp ;
  Conjunct = Conj ;
  [Command] = [Imp] ;
  Place = NP ;
  Route = Adv ;
  Action = VP ;
  Way = Prep ; -- in a certain way (adverb, or via some landmark)
  Direction = Adv ;
  Object = NP ;
  Determ = Det ;
  Thing = N ;
  Time = Adv ;

  Number = Det ;
  -- Number = NP ; -- from the arithmetic library
  -- 3 + 5 minutes
  -- 3 to 5 minutes

  --I gnore for now, just included to approximate completeness
  Question = QS ;
  Something = QCl ;

lin

  -- SimpleCom : Action -> Command ;
  SimpleCom a = mkImp a ;
  -- DriveTo : Action -> Way -> Place -> Command ;
  DriveTo a w p = mkImp (mkVP a (C.mkAdv w p)) ;

  -- MultipleRoutes : Conjunct -> [Command] -> Command ;
  MultipleRoutes = ConjImp ;

  -- ModAction : Action -> Direction -> Action ;
  ModAction a d = mkVP a d ;

  DoTil a t = mkImp (mkVP a t) ; --  "turn right in five minutes"

  -- ObjectPlace : Object -> Place ;
  ObjectPlace o = o ;

  -- WhichObject : Determ -> Thing -> Object ;
  WhichObject = mkNP ;

  And = and_Conj ;
  Or = or_Conj ;
  Then = then ;

  -- Hack : https://inariksit.github.io/gf/2019/01/26/literals-2.html
  -- MkNum : Int -> Number ;
  MkNum int =
    let sym : Symb = mkSymb int.s ; -- mkSymb : Str -> Symb ;
        card : Card = symb sym ;    -- symb : Symb -> Card ;
        det : Det = mkDet card ;
    in det ;

  -- InNMin : Determ -> Time ;
  InNMin d way = C.mkAdv way (mkNP d minutes) ;

  --List Constructors
  BaseCommand = BaseImp ;
  ConsCommand = ConsImp ;

  Now = ParadigmsEng.mkAdv "now" ;
  InFive = C.mkAdv in_Prep fiveminutes ;

  --  : Direction ;
  Left = ParadigmsEng.mkAdv "left" ;
  Right = ParadigmsEng.mkAdv "right" ;
  Around = ParadigmsEng.mkAdv "around" ;

  -- Way
  -- temporal vs spatial prepositions
  At = mkPrep "at" ;
  By = by8agent_Prep ;
  On = on_Prep ;
  In = in_Prep ;
  To = to_Prep ;
  From  = from_Prep ;
  After = after_Prep ;
  Under = under_Prep ;
  Over  = mkPrep "over" ;
  Before  = mkPrep "before" ;
  Until  = mkPrep "until" ;
  Past  = mkPrep "past" ;


  -- places
  Home = mkNP (mkN "home") ; -- fix "drive to home"
  Edinburgh = mkNP (mkPN "Edinburgh") ;
  London = mkNP (mkPN "London") ;
  Gothenburg = mkNP (mkPN "Gothenburg") ;

  -- action
  Drive = mkVP (mkV "drive") ;
  Go = mkVP (mkV "go") ;
  Stop = mkVP (mkV "stop") ;
  Break = mkVP (mkV "break") ;
  Turn = mkVP (mkV "turn") ;

  -- Determ
  A = a_Det ;
  The = the_Det ; --need to refactor to include both singular and plural
  This = this_Det ;
  These = these_Det ;
  That = that_Det ;

  Person = mkN "person" "people" ;
  Cafe = mkN "cafe" ;
  Gallery = mkN "gallery" ;
  Museum = mkN "museum" ;
  Bridge = mkN "bridge" ;
  Tree = mkN "tree" ;
  Car = mkN "car" ;

  --for QA, ignore for now
  -- Ask : Something -> Question ;
  Ask s = mkQS s ;
  -- Thing : Something ;
  SomeThing = mkQCl who_IP (mkV "something") ;

oper
  five    = mkDet (mkCard (mkNumeral n5_Unit)) ;
  minutes = mkN "minute" "minutes" ;
  fiveminutes = mkNP five minutes ;
  then = mkConj "then" ;

}
