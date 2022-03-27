addOne([], []).

addOne([H|T], [H,1|R]):-
    H2 is H mod 2,
    H2 =:= 0,
    addOne(T, R).

addOne([H|T], [H|R]):-
    H2 is H mod 2,
    H2 =:= 1,
    addOne(T, R).
