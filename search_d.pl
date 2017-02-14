search_d(_D,Goal,Goal):-
	goal(Goal). 
search_d(D,Current,Goal):-
	%writeln(Current),
	D>0,D1 is D-1,
	child(Current,Child),
	search_d(D1,Child,Goal). 

goal(N):-7 is N.
goal(N):-42 is N.

child(N,N+1).
child(N,2*N).

/** &lt;examples&gt;

?- search_d(8,0,Goal).
?- search_d(8,0,Goal),42 is Goal.

*/
