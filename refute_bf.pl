% refute_bf(Clause) <- Clause is refuted by clauses 
%                      defined by cl/1 
%                      (breadth-first search strategy)
refute_bf(Clause):-
	refute_bf_a([a(Clause,Clause)],Clause).

refute_bf_a([a((false:-true),Clause)|_Rest],Clause).
refute_bf_a([a(A,C)|Rest],Clause):-
	writeln(A),
	findall(a(R,C),(cl(Cl),resolve(A,Cl,R)),Children),
	append(Rest,Children,NewAgenda),	% breadth-first
	refute_bf_a(NewAgenda,Clause).

% resolve(C1,C2,R) <- R is the resolvent of C1 and C2.
resolve((H1:-B1),(H2:-B2),(ResHead:-ResBody)):-
	resolve(H1,B2,R1,R2),
	disj_append(R1,H2,ResHead),
	conj_append(B1,R2,ResBody).
resolve((H1:-B1),(H2:-B2),(ResHead:-ResBody)):-
	resolve(H2,B1,R2,R1),
	disj_append(H1,R2,ResHead),
	conj_append(R1,B2,ResBody).

resolve((A;B),C,B,E):-
	conj_remove_one(A,C,E).
resolve((A;B),C,(A;D),E):-
	resolve(B,C,D,E).
resolve(A,C,false,E):-
	conj_remove_one(A,C,E).

cl((bachelor(X);married(X):-man(X),adult(X))).
cl((has_wife(X):-man(X),married(X))).
cl((false:-has_wife(paul))).
cl((man(paul):-true)).
cl((adult(paul):-true)).


%%% auxiliary predicates

disj_append(A,B,C):-
    ( A=false    -> B=C
    ; A=(A1;A2) -> C=(A1;C2),disj_append(A2,B,C2)
    ; otherwise -> C=(A;B)
    ).

conj_append(A,B,C):-
    ( A=true    -> B=C
    ; A=(A1,A2) -> C=(A1,C2),conj_append(A2,B,C2)
    ; otherwise -> C=(A,B)
    ).

conj_remove_one(X,X,true):-	% single-element conjunction
	not(X=true),not(X=(_,_)).
conj_remove_one(X,(X,Ys),Ys).
conj_remove_one(X,(Y,Ys),(Y,Zs)):-
	conj_remove_one(X,Ys,Zs).

/** &lt;examples&gt;

?- refute_bf((false:-bachelor(X))).

*/
