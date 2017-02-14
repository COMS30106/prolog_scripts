% hanoi(N,A,B,C,Moves) <- Moves is the list of moves to 
%                         move N disks from peg A to peg C, 
%                         using peg B as intermediary peg 
hanoi(0,_,_,_,[]).
hanoi(N,A,B,C,Moves):-
	N>0, N1 is N-1,
	hanoi(N1,A,C,B,Moves1),
	hanoi(N1,B,A,C,Moves2),
	append(Moves1,[A -> C|Moves2],Moves).

hanoi(N,Moves):-
	hanoi(N,left,middle,right,Moves).

/** &lt;examples&gt;

?- hanoi(3,Moves).
?- hanoi(5,Moves).

*/
