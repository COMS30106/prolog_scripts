search_df([Goal|_Rest],Goal):-
	goal(Goal). 
search_df([Current|Rest],Goal):-
	%writeln([Current|Rest]),
	findall(C,child(Current,C),Children),
	append(Children,Rest,NewAgenda),
	search_df(NewAgenda,Goal). 

goal(N):-7 is N.
goal(N):-42 is N.

child(N,N+1).
child(N,2*N).

/** &lt;examples&gt;

?- search_df([0],Goal).
?- search_df([0],Goal),42 is Goal.

*/
