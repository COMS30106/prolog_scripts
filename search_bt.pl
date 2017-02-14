search_bt(Goal,Goal):-
	goal(Goal). 
search_bt(Current,Goal):-
	%writeln(Current),
	child(Current,Child),
	search_bt(Child,Goal). 

goal(N):-7 is N.
goal(N):-42 is N.

child(N,N+1).
child(N,2*N).

/** &lt;examples&gt;

?- search_bt(0,Goal).
?- search_bt(0,Goal),42 is Goal.

*/
