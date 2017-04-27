letter(a).
letter(b).
letter(c).

:- atom_length(hello, X).
:- atom_length('enchanted evening', 17).

:- atom_chars('',[]).
:- atom_chars([],['[', ']']).
:- atom_chars('iso',['i','s','o']).
:- atom_chars(A,['p','r','o','l','o','g']).
:- atom_chars(audley,A).
:- atom_chars('North',['N'|X]).

:- atom(atom).
:- atom('string').
:- atom(a(b)).
:- atom(Var).
:- atom([]).
:- atom(6).

:- letter(c).
:- letter(d).
:- assertz(letter(d)).
:- letter(c).
:- letter(d).

:- true; fail.
:- fail; true.
:- fail;fail.
:- true;true.

check_anon(_,g(X),f(_,Y,_)).

:- check_anon(X,Y,Z).

check_call(Y) :-
   X = f,
   Z = append(X,X,Y),
   call(Z).
   
:- check_call(X).


   