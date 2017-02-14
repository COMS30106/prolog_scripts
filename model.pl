cl((bachelor(X);married(X):-man(X),adult(X))).
cl((has_wife(X):-man(X),married(X))).
%cl((false:-has_wife(paul))).
cl((man(paul):-true)).
cl((adult(paul):-true)).

model(M0,M):-
   is_violated(Head,M0),!, % find instance of violated clause
   disj_element(L,Head),   % select ground literal from the head
   model([L|M0],M).        % and add it to the model
model(M,M).	% no more violated clauses

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

?- model([],M).

*/
