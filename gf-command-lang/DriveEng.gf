concrete DriveEng of Drive = open
    SyntaxEng,
    SymbolicEng,
    (C = ConstructorsEng),
    ParadigmsEng,
    ConjunctionEng,
    -- ExtraEng,
    ExtendEng,
    Prelude in {

lincat

  -- Route        = Text ;
  [Commands]   = Text ;
  Commands     = Text ;
  Polarity     = Pol ;
  Keyword      = Utt ;
  -- ListCommand  = Utt ;
  Command      = Utt ; --- { ut : Utt ; adv : Adv } ;
  PosCommand   = Imp ;
  Conjunct     = Conj ;
  [PosCommand] = [Imp] ;
  Place        = NP ;
  [Place]      = ConstructorsEng.ListNP ; --NP ;
  Action       = VP ;
  Way          = Prep ; -- in a certain way (adverb, or via some landmark)
  AdvPh        = Adv ;
  AdjPh        = Adv ;
  How          = Adv ;
  Where        = Adv ;
  Object       = NP ;
  [Object]     = ConstructorsEng.ListNP ;
  Determ       = Det ;
  UndetObj     = CN ;-- N ;
  Time         = Adv ;
  Descript     = SyntaxEng.A ; --adjective

  Condition = Cl ; --should be Utt?

  Number = Det ;
  -- Number = NP ; -- from the arithmetic library
  -- 3 + 5 minutes
  -- 3 to 5 minutes

  --I gnore for now, just included to approximate completeness
  Question = QS ;
  Something = QCl ;

lin

  -- SimpleCom : Action -> PosCommand ;
  SimpleCom a = mkImp a ;
  Finish = mkImp (mkV "Finish") ; -- make consistent with End of sentence in corpus

  --removed
  -- DriveTo : Action -> Way -> Place -> PosCommand ;
  -- DriveTo a w p = mkImp (mkVP a (C.mkAdv w p)) ;

  ConsCommands x xs = ss (x.s ++ xs.s) ;
  BaseCommands x = x ;

  -- OneCommand   : PosCommand -> Commands ;
  OneCommand pc = mkText (mkPhr (mkUtt pc)) fullStopPunct;
  -- ManyCommands : PosCommand -> Commands -> Commands ;
  -- ManyCommands pc cs = mkText (mkUtt pc) fullStopPunct cs ;

  -- MultipleRoutes : Conjunct -> [PosCommand] -> PosCommand ;
  CompoundCommand = ConjImp ;
  MultipleObject  = ConjNP ;
  MultiplePlaces  = ConjNP ;
  -- MultipleObject s1 s2 = mkNP and_Conj (mkListNP s1 s2) ;

  -- UnlessSomething : UndetObj -> Condition ;
  UnlessSomething t = mkCl t ;

  -- ModAction : Action -> Direction -> Action ;
  ModAction a d = mkVP a d ;

  -- MkAdjPh : Way -> Obj -> AdjPh ;
  MkAdjPh w p = C.mkAdv w p ;

  -- MkAdvPh : Way -> Place -> AdvPh ;
  MkAdvPh w p = C.mkAdv w p ;
  -- HowPhrase : How -> AdvPh ;
  HowPhrase how = how ;
  -- WherePhrase : where -> AdvPh ;
  WherePhrase wher = wher ;

  DoTil a t = mkImp (mkVP a t) ; --  "turn right in five minutes"

  -- ObjectPlace : Object -> Place ;
  ObjectPlace o = o ;

  -- WhichObject : Determ -> UndetObj -> Object ;
  WhichObject = mkNP ;

  -- ModifyCommand : Conjunct -> Condition -> PosCommand -> Command ; -- add polarity
  ModifyCommand conj cl imp =
      let clUtt : Utt = mkUtt cl ;
          impUtt : Utt = mkUtt imp ;
          fakeAdv : Adv =
            C.mkAdv conj
                  <impUtt : Adv>
                  <clUtt : Adv> ;
      in lin Utt fakeAdv ;

  -- ControlledCom : Conjunct -> Condition -> PosCommand -> PosCommand ;
  -- ControlledCom conj cond com = <(C.mkAdv conj <(mkUtt cond) : Adv> <(mkUtt com) : Adv>) : Imp> ;
  -- ControlledCom conj cond com = <(C.mkAdv conj <(mkUtt cond) : Adv> <(mkUtt com) : Adv>) : Imp> ;
  -- mkAdj conj <utt1 : Adv> <utt2 : Adv>


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
  -- BaseCommand = BaseAdv ;
  -- ConsCommand com1 l = ConsAdv ({s = com1.adv.s ++ com1.ut.s }) l ;

  BasePosCommand = BaseImp ;
  ConsPosCommand = ConsImp ;

  BaseObject = mkListNP ;
  ConsObject = mkListNP ;

  BasePlace = mkListNP ;
  ConsPlace = mkListNP ;

  -- ModObj : Descript -> UndetObj -> UndetObj ;
  ModObj = mkCN ;

  -- PhraseModObj : AdjPh -> UndetObj -> UndetObj ;
  PhraseModObj = mkCN ;


  And    = and_Conj ;
  Or     = or_Conj  ;
  Then   = then     ;
  Unless = unless   ;

  Now = ParadigmsEng.mkAdv "now" ;

  Carefully = ParadigmsEng.mkAdv "carefully" ;
  Quickly   = ParadigmsEng.mkAdv "quickly"   ;

  --  : Direction                          ;
  Left     = ParadigmsEng.mkAdv "left"     ;
  Right    = ParadigmsEng.mkAdv "right"    ;
  Around   = ParadigmsEng.mkAdv "around"   ;
  Straight = ParadigmsEng.mkAdv "straight" ;

  -- Way
  -- temporal vs spatial prepositions
  At     = mkPrep "at"     ;
  With   = with_Prep       ; -- mkPrep "at" ;
  By     = by8agent_Prep   ;
  On     = on_Prep         ;
  In     = in_Prep         ;
  To     = to_Prep         ;
  From   = from_Prep       ;
  After  = after_Prep      ;
  Under  = under_Prep      ;
  Over   = mkPrep "over"   ;
  Before = mkPrep "before" ;
  Until  = mkPrep "until"  ;
  Past   = mkPrep "past"   ;


  -- places
  Home       = mkNP (mkN "home") ; -- fix "drive to home"
  Edinburgh  = mkNP (mkPN "Edinburgh") ;
  London     = mkNP (mkPN "London") ;
  Gothenburg = mkNP (mkPN "Gothenburg") ;

  -- action
  Drive = mkVP (mkV "drive") ;
  Go    = mkVP (mkV "go")    ;
  Stop  = mkVP (mkV "stop")  ;
  Break = mkVP (mkV "break") ;
  Turn  = mkVP (mkV "turn")  ;

  -- Determ
  A     = a_Det     ;
  The   = the_Det   ; --need to refactor to include both singular and plural
  This  = this_Det  ;
  These = these_Det ;
  That  = that_Det  ;

  -- Descript
  Female    = invarA "female"     ;
  Male      = invarA "male"       ;
  Big       = mkA "big" "bigger"  ;
  Small     = mkA "small"         ;
  Fast      = mkA "fast"          ;
  Slow      = mkA "slow"          ;
  Living    = invarA "living"     ;
  Nonliving = invarA "non-living" ;

  Person  = mkCN (mkN "person" "people") ;
  Woman   = mkCN (mkN "woman" "women") ;
  Dog     = mkCN (mkN "dog") ;
  Cafe    = mkCN (mkN "cafe") ;
  Store   = mkCN (mkN "store") ;
  Gallery = mkCN (mkN "gallery") ;
  Museum  = mkCN (mkN "museum") ;
  Traffic = mkCN (mkN "traffic") ;
  -- different things
  Bridge  = mkCN (mkN "bridge") ;
  Tree    = mkCN (mkN "tree") ;
  Car     = mkCN (mkN "car") ;

  --TODO
  -- for QA, ignore for now
  -- Ask : Something -> Question ;
  Ask s = mkQS s ;
  -- UndetObj : Something ;
  SomeUndetObj = mkQCl who_IP (mkV "something") ;

  -- Keyword
  StartRoute = mkUtt (mkImp (mkV2 (mkV "start")) (mkNP route)) ;
  EndRoute   = mkUtt (mkImp (mkV2 (mkV "end")) (mkNP route)) ;

oper

  minutes = mkN "minute" "minutes" ;
  then    = mkConj "then"          ;
  unless  = mkConj "unless"        ;
  route   = mkN "route"            ;

-- oper
--   advIsUtt : Adv -> Utt ;
--   advIsUtt a = <a : Utt> ;
--   -- advIsUtt a = <a : Utt> ;
--   -- Adv -> Utt

}
