search_id(D,Current,Goal):-
	search_d(D,Current,Goal). 
search_id(D,Current,Goal):- 
	%writeln(Current),
	D1 is D+1,  % increase depth
	search_id(D1,Current,Goal). 

search_d(_D,Goal,Goal):-
	goal(Goal). 
search_d(D,Current,Goal):-
	D>0,D1 is D-1,
	child(Current,Child),
	search_d(D1,Child,Goal). 

goal(N):-7 is N.
goal(N):-42 is N.

child(N,N+1).
child(N,2*N).

/** &lt;examples&gt;

?- search_id(1,0,Goal).
?- search_id(1,0,Goal),42 is Goal.

*/
