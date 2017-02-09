% fibn(N,Na,Nb,F) <- F is the N-th Fibonacci number
%                    in the sequence starting with Na, Nb
fibn(1,Na,_,Na).
fibn(2,_,Nb,Nb).
fibn(N,Na,Nb,F):-
	N>2, N1 is N-1,
	Nc is Na+Nb,
	fibn(N1,Nb,Nc,F).

% the original Fibonacci sequence starts with 1, 1
fibn(N,F):-
	fibn(N,1,1,F).

/** &lt;examples&gt;

?- member(N,[10,20,50,100,200]), fibn(N,F).
?- maplist(fibn, [10,20,50,100,200], L).

*/
