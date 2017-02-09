prove(A):-
    ( A=true    -> true              % we're done
    ; A=not(B)  -> not(prove(B))     % negation as failure
    ; A=(B,C)   -> prove(B),prove(C) % conjunctive query
    ; otherwise -> cl(A,B),prove(B)  % resolve against clause
    ).

% cl(H,B) <- (H:-B) is object-level clause
cl(rectangle,(polygon(4),angles(90))).
cl(square,(rectangle,regular)).
cl(triangle,polygon(3)).
cl(equilateral_triangle,(triangle,regular)).
%%% facts
cl(polygon(4),true).
cl(regular,true).
cl(angles(90),true).

/** &lt;examples&gt;

?- prove(rectangle).
?- prove(square).
?- prove(not(triangle)).
?- prove(not((rectangle,triangle))).

*/
