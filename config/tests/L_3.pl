simple(x) :- false.
simple(x) :- true.

--  :- simple(x).

ex3(X,a).
ex3(b,X).
ex3(X,Y) :- ex3(X,a), ex3(b,Y).

:- ex3(t,q).