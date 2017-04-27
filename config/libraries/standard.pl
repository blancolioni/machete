'='(X,X).

'C'([H|T], H, T).

';'(X,_) :- X.
';'(_,Y) :- Y.

atom_chars(A,Cs) :- var(A), '-min-atom_chars'(A, Cs).
atom_chars(A,Cs) :- '-min-atom_chars'(A, MinChars), Cs = MinChars.

last([X], X).
last([_|T], X) :- last(T,X).

member(X,[X|_]).
member(X,[_|Z]) :- member(X,Z).

append([], Y, Y).
append([H|T], Y, Z) :- append (T, Y, Z1), [H|Z1] = Z.

dynamic(_).

findall( X, Goal, Xlist)  :- 
  call( Goal),                         % Find a solution
  assertz( queue(X) ),                 % Assert it
  fail;                                % Try to find more solutions 
  assertz( queue(bottom) ),            % Mark end of solutions 
  collect( Xlist).                     % Collect the solutions 
 
collect( L)  :-
  retract( queue(X) ), !,              % Retract next solution 
  ( X == bottom, !, L = []             % End of solutions?
    ;
    L = [X | Rest], collect( Rest) ).  % Otherwise collect the rest
