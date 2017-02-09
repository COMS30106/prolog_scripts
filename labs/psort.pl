:-use_module(library(random)). % for randseq/3 to generate random lists

% psort(L,SL) <- L and SL contain the same elements, SL is sorted
psort(L,SL):-
    perm(L,SL),   % generator
    write('. '),  % demonstrate inefficiency of generator
    sorted(SL).   % tester

% perm(L,PL) <- L and PL are permutations of each other
perm([],[]).
perm(L,[X|PR]):-
	remove_one(X,L,R),
	perm(R,PR).

% remove_one(X,Ys,Zs) <- Zs is Ys minus one occurence of X
remove_one(X,[X|Ys],Ys).
remove_one(X,[Y|Ys],[Y|Zs]):-
	remove_one(X,Ys,Zs).

% sorted(L) <- L is a sorted list
sorted([]).
sorted([_X]).
sorted([X,Y|L]):-
  ( compare('=', X, Y)
  ; compare('<', X, Y)
  ), sorted([Y|L]).

/** &lt;examples&gt;

?- randseq(5,100,In), psort(In,Out).

*/
