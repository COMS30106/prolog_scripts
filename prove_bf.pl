% breadth-first version of prove_r/1
% agenda items have the form a(CurrentQuery,OriginalQuery)
% in order to obtain an answer substitution on termination
prove_bf(Goal):-
	prove_bf_a([a(Goal,Goal)],Goal).

prove_bf_a([a(true,Goal)|_Agenda],Goal).
prove_bf_a([a((A,B),G)|Agenda],Goal):-!,
	findall(a(D,G),
	        (cl(A,C),conj_append(C,B,D)),
	        Children),
	append(Agenda,Children,NewAgenda),	% breadth-first
	prove_bf_a(NewAgenda,Goal).
prove_bf_a([a(A,G)|Agenda],Goal):-
	findall(a(B,G),cl(A,B),Children),
	append(Agenda,Children,NewAgenda),	% breadth-first
	prove_bf_a(NewAgenda,Goal).

% prove_r/1 included for comparison
prove_r(A):-
    ( A=true    -> true
    ; A=(B,C)   -> cl(B,D),conj_append(D,C,E),prove_r(E)
    ; otherwise -> cl(A,B),prove_r(B)
    ).

cl(brother(X,Y),brother(Y,X)).
cl(brother(X,Y),(brother(X,Z),brother(Z,Y))).
cl(brother(peter,paul),true).
cl(brother(adrian,paul),true).

%%% auxiliary predicates

conj_append(A,B,C):-
    ( A=true    -> B=C
    ; A=(A1,A2) -> C=(A1,C2),conj_append(A2,B,C2)
    ; otherwise -> C=(A,B)
    ).

/** &lt;examples&gt;

?- prove_bf(brother(peter,paul)).
?- prove_r(brother(peter,paul)).
?- prove_bf(brother(peter,Y)).
?- prove_r(brother(peter,Y)).

*/
