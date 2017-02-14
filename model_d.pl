cl((append([],Y,Y):-list(Y))).
cl((append([X|Xs],Ys,[X|Zs]):-thing(X),append(Xs,Ys,Zs))).
cl((list([]):-true)).
cl((list([X|Y]):-thing(X),list(Y))).
cl((thing(a):-true)).
cl((thing(b):-true)).
%cl((thing(c):-true)).

model_d(0,M,M).
model_d(D,M0,M):-
	D>0,D1 is D-1,
	findall(H,is_violated(H,M0),Heads),
	satisfy_clauses(Heads,M0,M1),
	model_d(D1,M1,M).

satisfy_clauses([],M,M).
satisfy_clauses([H|Hs],M0,M):-
	disj_element(L,H),
	satisfy_clauses(Hs,[L|M0],M).

is_violated(H,M):-
   cl((H:-B)),
   satisfied_body(B,M),	% this will ground the variables
   not(satisfied_head(H,M)).

satisfied_body(true,_M).	% body is a conjunction
satisfied_body(A,M):-
   member(A,M).
satisfied_body((A,B),M):-
   member(A,M),
   satisfied_body(B,M).

satisfied_head(A,M):-	% head is a disjunction
   member(A,M).
satisfied_head((A;_B),M):-
   member(A,M).
satisfied_head((_A;B),M):-
   satisfied_head(B,M).

disj_element(X,X):-	% single-element disjunction
	not(X=false),not(X=(_;_)).
disj_element(X,(X;_Ys)).
disj_element(X,(_Y;Ys)):-
	disj_element(X,Ys).

/** &lt;examples&gt;

?- model_d(4,[],M).

*/
