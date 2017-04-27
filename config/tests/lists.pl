list([]).
list(.(X,Y)) :- list(Y).

write_member_result :-
  member(X, [1,2,3]),
  write(X), nl,
  fail.
  
:- list([a,b]).
:- member(a,[a]).
:- member(a,[b,a]).
:- member(a,[b,c,d,e,f]).
:- member(X, [a,b,c]).
:- append([a],[],X).
:- append([a,b],[c,d],X).
:- append([a,b],[c,d],_).
:- append([a,b],[c,d],[a,b,c,d]).
:- append(X, Y, [a,b,c,d]).
:- write_member_result.
% :- append(X,Y,Z).
:- call(append([a],[b],[a,b])).