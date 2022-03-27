nrOcc([], _, 0).

nrOcc([H|T], E, C):-
    H =:= E,
    nrOcc(T, E, C2),
    C is C2 + 1.

nrOcc([H|T], E, C):-
    H =\= E,
    nrOcc(T, E, C).


isSet([], _).

isSet([H|T], P):-
    nrOcc(P, H, 1),
    isSet(T, P).

isSetMain(L):-
    isSet(L, L).


diff([], _, []).

diff([H|T], P, [H|R]):-
    nrOcc(P, H, 0),
    diff(T, P, R).

diff([H|T], P, R):-
    nrOcc(P, H, C),
    C>0,
    diff(T, P, R).


%diffMain(S1, _, []):-
%    not(isSetMain(S1)).

%diffMain(_, S2, []):-
%    not(isSetMain(S2)).

diffMain(S1, S2, R):-
    isSetMain(S1),
    isSetMain(S2),
    diff(S1, S2, R).
