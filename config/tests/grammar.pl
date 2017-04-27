sentence([S]) --> imperative(S).

imperative(Command) --> verb_phrase(Command).

verb_phrase(cmd(Command, D, IOs)) --> verb(Command), optional_direct_object(D), indirect_objects(IOs).
verb_phrase(cmd(Command, D, IOs)) --> [V,P], {multi_verb(Command,[V,P]) }, optional_direct_object(D), indirect_objects(IOs).
verb_phrase(cmd(Command, D, IOs)) --> [V], direct_object(D), [P], {multi_verb(Command,[V,P]) }, indirect_objects(IOs).

optional_direct_object(D) --> direct_object(D).
optional_direct_object(none) --> [].

indirect_objects([H|T]) --> indirect_object(H), indirect_objects(T).
indirect_objects([]) --> [].

direct_object(D) --> determiner_phrase(D).

indirect_object(O) --> relation(R), determiner_phrase(O).

%determiner_phrase(NP, X,Y) :- determiner(G, X, X1), noun_phrase(NP, X1, Y).
determiner_phrase(NP) --> determiner(G), noun_phrase(NP).

noun_phrase(np(Premods,Noun,Postmods)) --> premodifiers(Premods), noun(Noun), postmodifiers(Postmods).

premodifiers([H|T]) --> premodifier(H), premodifiers(T).
premodifiers([]) --> [].

postmodifiers([H|T]) --> postmodifier(H), postmodifiers(T).
postmodifiers([]) --> [].

premodifier(X) --> adjective(X).

postmodifier(X) --> adjective_phrase(X).

adjective_phrase(X) --> preposition(P), noun_phrase(X).

conjunction --> [and].

relation(by) --> [by].

verb(choose) --> [pick].
verb(find) --> [find].

noun(block, [block|X], X).
noun(pyramid) --> [pyramid].
noun(ball) --> [ball].
noun(table) --> [table].

adjective(colour(red)) --> [red].
adjective(colour(green)) --> [green].
adjective(colour(blue)) --> [blue].

adjective(size(large)) --> [big].
adjective(size(small)) --> [small].

determiner(g(definite,Number,Gender), [the|X], X).

preposition(up) --> [up].

multi_verb(pick_up, [pick,up]).

log(M) --> { write(M), nl }.

:- determiner_phrase(D,[the,block],[]).
:- noun(X, [block],[]).
:- verb(Command,[pick],[]).
:- verb_phrase(Command,[pick,up],[]).
:- noun_phrase(NP,[block],[]).
:- sentence(Command,[pick,up,the,block],[]).
:- sentence(Command,[pick,up,the,big,blue,block],[]).
