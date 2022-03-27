nrOcc([], _, 0).

nrOcc([H|T], E, C):-
    H =:= E,
    nrOcc(T, E, C2),
    C is C2 + 1.

nrOcc([H|T], E, C):-
    H =\= E,
    nrOcc(T, E, C).


diff([], _, []).

diff([H|T], P, [H|R]):-
    nrOcc(P, H, 0),
    diff(T, P, R).

diff([H|T], P, R):-
    nrOcc(P, H, C),
    C>0,
    diff(T, P, R).
