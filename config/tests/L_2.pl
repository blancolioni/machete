p(X,Y) :-
  q(X,Z),
  r(Z,Y).

q(a,b).
r(b,c).
 
:- p(U,V).