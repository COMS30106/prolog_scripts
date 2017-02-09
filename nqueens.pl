% :-use_rendering(chess). % Uncomment this line for chess renderer

nqueens(N,Columns) :-
    setof(X,between(1,N,X),Rows),
    perm(Rows,Columns), % generator
    safe(Columns).      % tester

%%% tester
safe([]).
safe([Column|Columns]) :-
    noattack(Column,Columns,1),
    safe(Columns).
    
noattack(_,[],_).
noattack(Y,[Y1|Ylist],Xdist) :-
    abs(Y-Y1) =\= Xdist,
    Xdist1 is Xdist+1,
    noattack(Y,Ylist,Xdist1).

%%% generator
perm([],[]).
perm(L,[X|PR]):-
	remove_one(X,L,R),
	perm(R,PR).
    
remove_one(X,[X|Ys],Ys).
remove_one(X,[Y|Ys],[Y|Zs]):-
	remove_one(X,Ys,Zs).

/** &lt;examples&gt;

?- nqueens(8,Columns).

*/
