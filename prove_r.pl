prove_r(A):-
    writeln(A), % show current query
    ( A=true    -> true
    ; A=not(B)  -> not(prove_r(B))
    ; A=(B,C)   -> cl(B,D),conj_append(D,C,E),prove_r(E)
    ; otherwise -> cl(A,B),prove_r(B)
    ).

conj_append(A,B,C):-
    ( A=true    -> B=C
    ; A=(A1,A2) -> C=(A1,C2),conj_append(A2,B,C2)
    ; otherwise -> C=(A,B)
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

?- prove_r(rectangle).
?- prove_r(square).
?- prove_r(not(triangle)).
?- prove_r(not((rectangle,triangle))).

*/
