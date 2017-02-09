% fib(N,F) <- F is the N-th Fibonacci number
% inefficient doubly-recursive version
fib(1,1).
fib(2,1).
fib(N,F):-
	N>2,N1 is N-1,N2 is N-2,
	fib(N1,F1),fib(N2,F2),
	F is F1+F2.

/** &lt;examples&gt;

?- fib(5,F).
?- fib(10,F).
?- fib(20,F).
?- fib(25,F).
?- fib(30,F).

*/
