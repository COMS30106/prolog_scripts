% list of pairs
p1(L,X,Y):-
	bagof((X,Y),(between(1,3,X),between(X,3,Y)),L).

% one list for each X
p2(L,X,Y):-
	bagof(Y,(between(1,3,X),between(X,3,Y)),L).

% lists for each X combined
p3(L,X,Y):-
	bagof(Y,X^(between(1,3,X),between(X,3,Y)),L).

% one list for each Y
p4(L,X,Y):-
	bagof(X,(between(1,3,X),between(X,3,Y)),L).

% lists for each Y combined
p5(L,X,Y):-
	bagof(X,Y^(between(1,3,X),between(X,3,Y)),L).

/** &lt;examples&gt;

?- p1(L,X,Y).
?- p2(L,X,Y).
?- p3(L,X,Y).
?- p4(L,X,Y).
?- p5(L,X,Y).

*/
