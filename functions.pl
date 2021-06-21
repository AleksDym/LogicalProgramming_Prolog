/* Praktikum 2 */
% Lists

% is_a_list(L) - Überprüft ob L eine Liste ist. 
is_a_list(L):-
L=[],!. %1: Eine leere Liste ist eine Liste --> return true =)
is_a_list([_|T]) :- 
  is_a_list(T),!. %3: Falls der Input eine Liste ist, überprüfe den Rest. 
is_a_list(_):-
  !,fail. % 2: Falls der Input keine Liste ist, dann return false =(

% diffList
diffList([], _, []) :- !. %1: Abbrechbedingung = wenn die erste Liste leer ist, dann nichte mehr weiter gehen.

diffList([Element|LRest], LDiff, Result) :-
  member(Element, LDiff), !, %2: Falls Element in LDiff ist, soll es nicht angehängt werden 
  diffList(LRest, LDiff, Result). %3: Fahre mit dem Rest der Liste fort. 

diffList([Element|LRest], LDiff, Result) :-
  Result = [Element | ResultRest], %2: Falls Element nicht in LDiff ist hänge es an die Ergebnisliste an
  diffList(LRest, LDiff, ResultRest). %3: Fahre mit dem Rest der Liste fort. 

% praefix
praefix([], _):- !,fail. %1: Pruefe, ob erste Liste leer ist, falls ja, dann fail
praefix(L1,L2):- length(L1,X), length(L2,Y),X>=Y,!,fail. %2: Pruefe, ob die erste Liste nicht gleich oder laenger als zweite Liste
praefix(L1,L2):- ppraefix(L1,L2).

ppraefix([], []):- !, fail. %3: Loop - nehme erstes Element von beiden Listen und vergleiche Sie, wenn beiden Listen leer sind, dann fai
ppraefix([], _L). %3: wenn erste Liste bis zum Ende iteriern und die Bedingung erfuellt ist, dann sind wir fertig
ppraefix([KInput|RInput], [KInput|RList]):- 
    ppraefix(RInput,RList),!.  %3: So lange die erten Elemente von beiden Listen gleich sind, iterieren wir weiter mit Rest.
    
% suffix

suffix(Suffix,List):- 
  length(Suffix,L1),
  length(List,L2), 
  L1>=L2,
  !,fail.%2: Pruefe, ob die erste Liste nicht gleich oder laenger als zweite Liste

suffix(Suffix,List):- 
  length(Suffix,L1),
  length(List,L2), 
  L1<L2, 
  suff(Suffix,List).%2: wenn nein, dann gehe weiter

suff([],[]). %3,4: Abbrechbedingung = wenn die Listen leer sind, dann nicht mehr weiter gehen.

suff([S|Suffix], [B|List]):-  %4:sobald beiden Listen gleiche Lange haben, dann entfernen wir die Elementen von beiden. 
  length(Suffix,L1),
  length(List,L2), 
  L1=L2, 
  suff(Suffix,List), 
  S=B. %4: wenn wir die Abbrechbedingung erreichen, gehen wir zurueck und fuegen die letzt geloeschten Elemente in beiden Listen ein. 
% und pruefen, ob die beide gleich sind. 
suff([EL|Suffix], [_B|List]):- %3: um Rekursion wzu nutzen und die Elemente asu beiden Listen von Ende zu pruefen, 
%sollen wir erstmal die Listen gleich lang machen: deswegen wenn die 1. Liste immer noch kleiner ist, entfernen wir die Elemente von 2.Liste
  length(Suffix,L1),
  length(List,L2), 
  L1<L2, 
  suff([EL|Suffix],List).


% infix

infix([],[]):- fail.
infix(_L,[]):- fail.
infix([],_):- fail. %1: pruefe, ob die Listen uberhaupt Elementen enthalten

infix(Inf,List):- 
  length(Inf,L1),
  length(List,L2), 
  L1>=L2, 
  !, fail. %2: Pruefe, ob die erste Liste nicht gleich oder laenger als zweite Liste


infix(Inf,List):- 
  length(Inf,L1),
  length(List,L2), 
  L1<L2, %2: Pruefe, ob die erste Liste nicht gleich oder laenger als zweite Liste
  is_sublist(Inf,List), 
  \+ suffix(Inf,List), %5: pruefe, ob die Liste kein Suffix und kein Praefix ist
  \+ praefix(Inf,List),!.%5: pruefe, ob die Liste kein Suffix und kein Praefix ist

% Helferfunktion is_sublist
is_sublist([El|Sub],[El|List]):- 
  checksublist(Sub,List). %4: falls finden wir, dass zwei Elemente von Listen gleich sind, 
  %nutzen wir die Funktion checksublist um sicher zu stellen, dass auch die weitere Elemente gleich werden. 
  %falls nicht-gehen mit Backtraching zurueck zur Funktion is_subList

is_sublist(Sub,[_L|List]):- 
  is_sublist(Sub,List). %3: loop: iteriere durch Elementen von beiden Listen so lange sie nicht gleich sind

% Helferfunktion checksublist
checksublist([], _L). %4 pruefen, ob alle Elemente von Liste in zweiter Liste zu enthalten sind
checksublist([El|Sub],[El|List]):- 
  checksublist(Sub,List).

% eo_count(L, E, O) -- Zählt die Anzahl der Geraden und ungeraden Listen innerhalb von L in E und O
% Helferfunktion evalList(L, E, O) - Überprüft ob ein Input eine geradde, ungerade oder gar keine Liste ist. 
% Speichert in E, O eine 1 falls even oder odd, Falls keine Liste, bei beiden 0.
evalList(L, E, O):-
  is_list(L), %1
  length(L, Length),
  EO is mod(Length, 2), %2: überprüft mit dem Modulo ob die Listenlänge gerade ist
  EO = 0,
  E = 1, %3 setzt E und O entpsrechend
  O = 0,!.
evalList(L, E, O):-
  is_list(L), %1
  length(L, Length),
  EO is mod(Length, 2), %2: überprüft mit dem Modulo ob die Listenlänge gerade ist
  EO = 1,
  E = 0, %3 setzt E und O entpsrechend
  O = 1,!.
evalList(_, E, O):-
  E = 0, %3 setzt E und O entpsrechend
  O = 0,!.

eo_count(L, Even, Odd):-
  eo_count_it(L, 0, 0, Even, Odd). %1: Erstellt die iterative Version bei der der Zustand von E und O immer gespeichert wird. 

eo_count_it(L, EI, OI, EO, OO):-
  evalList(L, E, O), % 2: wertet zuerst die Eingangsliste aus
  recL(L, EI, OI ,ET, OT),% 3: Überprüft rekursiv jedes Element der Eingangsliste
  EO is ET + E, %7: Inkrementiert E und O entsprechend. 
  OO is OT + O.

recL([EL|R], EI, OI, EO, OO):-
  eo_count_it(EL, EI, OI, ET, OT), %5: Wertet jedes Element der Liste auf die gleiche Weise aus. 
  recL(R, ET, OT, EO, OO),!. %6: Fährt mit dem Rest dieser Liste fort. 
recL(_, EI, OI, EO, OO):- % 4: Falls keine Liste vorhanden war oder diese leer ist, ändere nichts an E und O.
  EO = EI,
  OI = OO,!.

/*
% del_element
*/
% Helferfunktion indexes(L, E, IX) -- Schreibt alle Indexe eines Elementes E in einer Liste L in die Liste IX
indexes(L, E, IX):-
  indexes_it(L, E, IX, 1). %1: Führt die iterative Variante aus bei der die Laufvariable i mitgenommen wird und dann über die gesamte Liste L iteriert wird. 

indexes_it([], _, IXL, _):-IXL = [],!. %2: Falls L leer ist gehe die Rekursion zurück (bzw. gebe eine leere Liste zurück. )
indexes_it(L, E, IXL, I):-
  L = [EL | LRest],
  EL = E, %3: Falls Das momentane Element == E ist füge den momentanen Index in die Indexliste ein. 
  IXL = [I | IXRest],
  INew is I +1,
  indexes_it(LRest, E, IXRest, INew),!.
indexes_it(L, E, IXL, I):-
  L = [_ | LRest], %4: Falls das momentane Element != E ist fahre fort. 
  INew is I +1,
  indexes_it(LRest, E, IXL, INew),!.

% Helferfunktion: sublist(L, Start, End, R) - Erstellt eine Sublist von L die bei index Start anfängt und bis Index Ende geht und schreibe diese Liste in R.
sublist(L, Start, End, R):-
  sublist_it(L, Start, End, R, 1). %1: Führe die iterative Variante von sublist aus welche immer die Laufvariable i mitnimmt und dann die gesamte Liste L iteriert. 

sublist_it([], _, _, R, _):-  %2: Falls L leer ist gehe die Rekursion zurück (bzw. gebe eine leere Liste zurück. )
  R = [].
sublist_it(L, Start, End, R, I):-
  I >= Start, %3: Falls das Element zwischen Start und End ist füge es in die Ergebnisliste ein. 
  I < End,
  L = [EL | RestIn],
  R = [EL | RestOut],
  INew is I +1,
  sublist_it(RestIn, Start, End, RestOut, INew),!.
sublist_it(L, Start, End, R, I):-
  (I < Start;
  I >= End), %3: falls das Ergebnis nicht zwischen Start und End ist, fahre fort. 
  L = [_ | RestIn],
  INew is I +1,
  sublist_it(RestIn, Start, End, R, INew),!.

% Helferfunktionen cutList -- zwei Mini Funktionen die Eine Liste zu ihrem ersten bzw. letzten Element beschneiden. 
cutListToFirst(L, R):-
  sublist(L, 1, 2, R).

cutListToLast(L, R):-
  length(L, Length),
  End is Length +1,
  sublist(L, Length, End, R).

% Helferfunktion delAllIndexes(L, IXL, Result): Löscht alle Elemente in L deren Index in IXL steht. 
delAllIndexes(L, IX, LN):-
  delAllIndexes_it(L, IX, LN, 1). % 1: Führe die iterative Variante aus welche immer die Laufvariable i mitnimmt und dann die gesamte Liste L iteriert. 

delAllIndexes_it([], _ ,[], _). %2: Falls L leer ist gehe die Rekursion zurück (bzw. gebe eine leere Liste zurück. )
delAllIndexes_it(L, IX, Out_Liste, I):-
  L = [_ | Rest],
  member(I, IX), %3: Falls der momentane Index in IX enthalten ist, fahre fort.
  INew is I +1,
  delAllIndexes_it(Rest, IX, Out_Liste, INew),!.
delAllIndexes_it(L, IX, Out_Liste, I):-
  L = [EL | Rest], %3: Falls der momentane Index nicht in IX enthalten ist, füge das momentane Element der Ergebnisliste an. 
  INew is I +1,
  Out_Liste = [EL| RestOut],
  delAllIndexes_it(Rest, IX, RestOut, INew).

% del_element(P, E, L, R) löscht Elemente E in Liste L je nach Eingabe bei P e - das erste Auftreten, l - das letzte Auftreten, a - alle Auftreten
del_element(e, E, L, R):-
  indexes(L, E, IXL), %1: Erstelle Indexe von Element E in L
  cutListToFirst(IXL, IX), %2: Behalte nur den ersten Index in der Indexliste
  delAllIndexes(L, IX, R). %5: lösche den ersten Index. 

del_element(l, E, L, R):-
  indexes(L, E, IXL), %1: Erstelle Indexe von Element E in L
  cutListToLast(IXL, IX), %3: Behalte nur den letzten Index in der Indexliste
  delAllIndexes(L, IX, R). %5: Lösche den letzten Index

del_element(a, E, L, R):-
  indexes(L, E, IX), %1: Erstelle Indexe von Element E in L
  delAllIndexes(L, IX, R). %4,5: Lösche alle Indexe. 

% substituteAllIndexes(E2, L, IXL, R) - Tauscht alle Elemente in L deren Index in L steht mit E2 aus. 
substituteAllIndexes(E2, L, IXL, R):-
  substituteAllIndexes_it(E2, L, IXL, R, 1). % 1: Führe die iterative Variante aus welche immer die Laufvariable i mitnimmt und dann die gesamte Liste L iteriert. 

substituteAllIndexes_it(_, [], _, [], _). %2: Falls L leer ist gehe die Rekursion zurück (bzw. gebe eine leere Liste zurück. )
substituteAllIndexes_it(E2, L, IXL, R, I):-
  member(I, IXL), %3: Falls der momentane Index in IX enthalten ist, Füge E2 der Ergebnisliste an.
  L = [_ | RestIn], 
  R = [E2 | RestOut],
  INew is I +1, %4: Fahre fort mit dem Rest  der Liste
  substituteAllIndexes_it(E2, RestIn, IXL, RestOut, INew),!.
substituteAllIndexes_it(E2, L, IXL, R, I):-
  L = [El | RestIn], %3: Falls der momentane Index nicht in IX enthalten ist, füge das momentane Element der Ergebnisliste an. 
  R = [El | RestOut],
  INew is I +1, %4: Fahre fort mit dem Rest  der Liste
  substituteAllIndexes_it(E2, RestIn, IXL, RestOut, INew),!.

% substitute - Tauscht Elemente E1 in Liste mit E2 in L aus, je nach Eingabe bei P e - das erste Auftreten, l - das letzte Auftreten, a - alle Auftreten
substitute(e, E1, E2, L, R):- 
  indexes(L, E1, IXL), %1: Erstelle Indexe von Element E in L
  cutListToFirst(IXL, IX), %2: Beschneidet die IndexListe auf ihr erstes Element 
  substituteAllIndexes(E2, L, IX, R). %5: Tauscht am ersten Index aus

substitute(l, E1, E2, L, R):-
  indexes(L, E1, IXL), %1: Erstelle Indexe von Element E in L
  cutListToLast(IXL, IX), %3: Beschneidet die IndexListe auf ihr letztes Element
  substituteAllIndexes(E2, L, IX, R). % 5: Tauscht am letzten Index aus. 

substitute(a, E1, E2, L, R):-
  indexes(L, E1, IXL), %1: Erstelle Indexe von Element E in L
  substituteAllIndexes(E2, L, IXL, R). %4,5: Tauscht an allen Indexes aus.


% Natural numbers
% s Zahlen 
nat(0).
nat(s(X)) :- nat(X).

% s2nat
s2nat(0,X) :- X = 0. %1: Abbrechbedingung: wenn szahl schon null ist, dann ist die natzahl auch null
s2nat(s(S), Res) :-  %2: loop: so lange natnum nict null ist, inkrementieren wir die szahl.
  s2nat(S, RTmp),
  Res is RTmp +1.

% nat2s 
nat2s(0,0) :- !. %1: Abbrechbedingung
nat2s(Num, SNum) :- %2: loop: dekrementiere natzahl und inkrementiere szahl bevor die Abbrechbedingung erreicht ist
  DecrementNum is Num -1,
  nat2s(DecrementNum, SNumTemp),
  SNum = s(SNumTemp). 

% add
add(S1, 0, Sum):- %0: falls eine von zwei Zahlen 0 ist, dann ist das Ergebniss die andere Zahl
  Sum = S1,!.
add(0, S2, Sum):-
  Sum = S2,!.

add(0,0, Sum):-%0: falls beide Zahlen 0 sind, dann ist das Ergebinss auch 0
  Sum = 0,!.

add(S1, s(S2), Sum):- %1: addiere zweite Zahl zu erster Zahl, dann dekrementiere zweite Zahl und imkrementiere die Summe
  add(S1, S2, SumTemp),
  Sum = s(SumTemp).

% sub
sub(0, _, Differenz):-%1: falls erste Zahl  0 ist, dann ist das Ergebniss 0
  Differenz = 0,!.

sub(Minuend, 0, Differenz):-%2: falls zweite Zahl  0 ist, dann ist das Ergebniss zweite Zahl
  Differenz = Minuend,!.


sub(Minuend, s(Subtrahend), Differenz):- %3: loop: dekrementiere zweite Zahl, dekrementiere Differenz
  sub(Minuend, Subtrahend, DifferenzTemp),
  s(Differenz) = DifferenzTemp,!.

sub(_, _, 0):-  !. %3: Abbrechbedingung

% mul
mul(0, _, Produkt):- %1: wenn eine Zahl 0 ist, dann ist das Ergebnis 0
  Produkt=0,!.

mul(_, 0, Produkt):-%1: wenn eine Zahl 0 ist, dann ist das Ergebnis 0
  Produkt=0,!.

mul(Faktor1, Faktor2, Produkt):-
  Faktor1U = Faktor1, %2: initialisiere neue Variable fuer Counter und nutze die Hilfsfunktion mit 4 Inputs
  mulrec(Faktor1U, Faktor1, Faktor2, Produkt).

mulrec(_, Faktor1, s(0), Produkt):-%3: Abbrechbedingung
  Produkt = Faktor1,!.

mulrec(Faktor1U, Faktor1, s(Faktor2), Produkt):- %4: dekrementiere Faktor2 
  mulrec(Faktor1U, Faktor1, Faktor2, ProduktTemp),
  add(Faktor1U, ProduktTemp, Produkt). %4: und wenn die Abbrechbedingung erreicht ist, dann addiere Produkt und Counter variable

% Power
power(0, _, Result):-
  Result=0,!. % 1:  0 hoch etwas ist immer 0
power(_, 0, Result):-
  Result=s(0),!. %2: etwas hoch 0 ist immer 1
power(Base, Exponent, Result):-
  BaseU = Base, %3: Schreibe den ursprünglichen Wert von Base in BaseU
  powerrec(BaseU, Base, Exponent, Result).

powerrec(_, Base, s(0), Result):-
  Result = Base,!. % 4: Falls der Exponent 1 ist, ist das Ergebnis Base. (bzw. Die Rekursion wird rückwärts aufgelöst. )
powerrec(BaseU, Base, s(Exponent), Result):-
  powerrec(BaseU, Base, Exponent, ResultTemp), %5: Gehe solange in die Rekursion bis der Exponent 0 ist und mulitpliziere so oft mit Base U
  mul(BaseU, ResultTemp, Result).

% fac(N, Result) - Berechnet die Fakultät von N.
fac(0, R):- %1: Die Fakultät von 0 ist als 1 definiert. 
  R = s(0),!.
fac(N, R):- %3: Startet damit N * das Eregbnis von facrec zu bestimmen (facrec dekrementiert N in jedem Schritt zunächst.)
  facrec(N, RTemp),
  mul(N, RTemp, R).
facrec(s(0), R):- %2: Die Faultät von 1 ist auch als 1 definiert. 
  R = s(0),!.
facrec(s(N), R):- %3: Subtrahiere solange 1 von N bis N == 1 ist und multipliziere mit dem Ergebnis. 
  facrec(N, RTemp), 
  mul(RTemp, N, R).

% lt(N1, N2) - Überprüft ob N1 kleiner als N2 ist. 
lt(N1, N2):-
  N1 == N2, %1: Falls N1 == N2 ist N1 nicht kleiner
  !, fail.
lt(N1, N2):-
  sub(N2, N1, Res),
  Res == 0, %2: Falls N2 - N1 == 0 dann war N1 größer als N2, sonst ist N1 größer als N1
  !, fail.
lt(_, _):-
  !.

% mods(N, M, Result) - Berechnet den Modulo M einer s Zahl N in Result
mods(_, 0, _):-
  !, fail. % 1: der Modulo von 0 ist nicht definiert
mods(N, M, Result):-
  lt(N, M), %2: Falls N < M dann ist N der Modulo
  Result = N,!.
mods(N, M, Result):-
  sub(N, M, ResultTemp), %3: Subtrahiere M solange von N bis N < M
  mods(ResultTemp, M, Result).
