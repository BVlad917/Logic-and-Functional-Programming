addOne([], []).

addOne([H|T], [H|R]):-
    H2 is H mod 2,
    H2 =:= 0,
    addOne(T, R1),
    R=[1|R1].

addOne([H|T], [H|R]):-
    H2 is H mod 2,
    H2 =:= 1,
    addOne(T, R).
