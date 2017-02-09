% :-use_rendering(svgtree). % uncomment this line to see visualisations

% fibt(N,E) <- E is an arithmetic expression
% representing the N-th Fibonacci number
% that can be rendered as a tree
% and evaluated with is/2.
fibt(1,1).
fibt(2,1).
fibt(N,(F1+F2)):-
	N>2,N1 is N-1,N2 is N-2,
	fibt(N1,F1),fibt(N2,F2).

/** &lt;examples&gt;

?- fibt(5,E), F is E.
?- fibt(10,E), F is E.

*/
