%isElemInList([], _):- false.

%isElemInList([H], E):- 
%    H =:= E.

isElemInList([H|_], E):-
    H =:= E.

isElemInList([H|T], E):-
    H =\= E,
    isElemInList(T, E).


diff([], _, []).

diff([H|T], P, R):-
    isElemInList(P, H),
    diff(T, P, R).

diff([H|T], P, [H|R]):-
    not(isElemInList(P, H)),
    diff(T, P, R).

