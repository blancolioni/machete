dcg(x) --> [X], { atom_length(X,4) }.
dcg(y) --> [a,b].

as --> [].
as --> [a], as.

abs --> [].
abs --> [a,b], abs.


:- dcg(Y,[a,b],[]).
:- dcg(X,[asdf],[]).
:- as([a],[]).
:- abs([X,Y,Z,b],[]).

/*
sen --> np, vp.

np --> n.

vp --> v.

n([dog|T],T).

v([barks|T],T).

:- sen([dog|X], Y).
:- sen(X,[]).
*/