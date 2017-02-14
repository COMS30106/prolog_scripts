search_bf([Goal|_Rest],Goal):-
	goal(Goal). 
search_bf([Current|Rest],Goal):-
	%writeln([Current|Rest]),
	findall(C,child(Current,C),Children),
	append(Rest,Children,NewAgenda),
	search_bf(NewAgenda,Goal). 

goal(N):-7 is N.
goal(N):-42 is N.

child(N,N+1).
child(N,2*N).

/** &lt;examples&gt;

?- search_bf([0],Goal).
?- search_bf([0],Goal),42 is Goal.

*/
