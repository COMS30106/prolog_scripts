biglist(Low,High,L):-
    bagof(X,between(Low,High,X),L).

% between/3 is built-in but could have been defined as follows:
% between(Low,_High,Low).
% between(Low,High,Number):-
% 	Low < High,
% 	NewLow is Low+1,
% 	between(NewLow,High,Number).

/** &lt;examples&gt;

?- biglist(1,25,L).

*/
