path(SS):-
	width(W),height(H),empty_grid(W,H,Grid),
	setx_hor(5/3,4,Grid),setx_hor(7/6,2,Grid),setx_hor(3/9,4,Grid),
	setx_ver(2/5,3,Grid),setx_ver(4/4,3,Grid),setx_ver(7/7,2,Grid),
	search(Grid,SS),show_grid(Grid).

width(8).       height(10).
entrance(1/1).  exit(8/5).   %exit(3/5).

/*
 * Search predicates
 * move/2 enumerates all possible moves, ignoring whether cell is free
 * search/2 searches grid with various search strategies
 */

:-use_module(library(lists)).

moves(X/Y,Moves):-
	bagof(X1/Y1,move(X/Y,X1/Y1),Moves).

move(X/Y,X/Y1):-
	height(H),
	H>Y,Y1 is Y+1.	% down
move(X/Y,X1/Y):-
	width(W),
	W>X,X1 is X+1.	% right
move(X/Y,X/Y1):-
	Y>1,Y1 is Y-1.	% up
move(X/Y,X1/Y):-
	X>1,X1 is X-1.	% left

% for A* search: generates moves with increasing F-value
moves_h(F/G:X/Y,Moves):-
	setof(F1/G1:X1/Y1,move_h(F/G:X/Y,F1/G1:X1/Y1),Moves).

move_h(_/G:X/Y,F1/G1:X1/Y1):-
	move(X/Y,X1/Y1),
	h(X1/Y1,H1),
	G1 is G+1,	% increase G-value from previous
	F1 is G1+H1.	% add new H-value

h(X/Y,H):-
	exit(XE/YE),
	H is abs(XE-X)+abs(YE-Y).	% Manhattan distance
	

search(Grid,SS):-
	entrance(X/Y),
	( SS=bt -> search_bt(0,X/Y,Grid)	% backtracking search
	; SS=df -> search_df(0,[X/Y],Grid)	% agenda-based depth-first
	; SS=bf -> search_bf(0,[X/Y],Grid)	% agenda-based breadth-first
	; SS=as -> h(X/Y,H),search_as(0,[H/0:X/Y],Grid)	% A* search
	).

% backtracking search
% only path found is indicated in grid
search_bt(N,X/Y,Grid):-
	exit(X/Y),
	set(X/Y,Grid,N).
search_bt(N,X/Y,Grid):-
	N1 is N+1,
	set(X/Y,Grid,N),
	move(X/Y,X1/Y1),
	search_bt(N1,X1/Y1,Grid).

% agenda-based depth-first search
% grid is used for loop-detection: if set/3 fails, it is either an obstacle
% or we've been there already
search_df(N,[X/Y|_],Grid):-
	exit(X/Y),
	set(X/Y,Grid,N).
search_df(N,[X/Y|Agenda],Grid):-
	( set(X/Y,Grid,N) -> moves(X/Y,Moves),N1 is N+1,
	                     append(Moves,Agenda,NewAgenda)
	; otherwise -> NewAgenda=Agenda,N1=N	% ignore: can't go there
	),search_df(N1,NewAgenda,Grid).

% agenda-based breadth-first search
search_bf(N,[X/Y|_],Grid):-
	exit(X/Y),
	set(X/Y,Grid,N).
search_bf(N,[X/Y|Agenda],Grid):-
	( set(X/Y,Grid,N) -> moves(X/Y,Moves),N1 is N+1,
	                     append(Agenda,Moves,NewAgenda)
	; otherwise -> NewAgenda=Agenda,N1=N	% ignore: can't go there
	),search_bf(N1,NewAgenda,Grid).

% A* search: agenda contains F/G:X/Y with F=G+H evaluation of position X/Y
search_as(N,[_:X/Y|_],Grid):-
	exit(X/Y),
	set(X/Y,Grid,N).
search_as(N,[F/G:X/Y|Agenda],Grid):-
	( set(X/Y,Grid,N) -> moves_h(F/G:X/Y,Moves),N1 is N+1,
	                     merge_h(Moves,Agenda,NewAgenda)
	; otherwise -> NewAgenda=Agenda,N1=N	% ignore: can't go there
	),search_as(N1,NewAgenda,Grid).

merge_h([],Agenda,Agenda).
merge_h(Moves,[],Moves).
merge_h([F/G:X/Y|Moves],[F1/G1:X1/Y1|Agenda],[F/G:X/Y|NewAgenda]):-
	F1>F,
	merge_h(Moves,[F1/G1:X1/Y1|Agenda],NewAgenda).
merge_h([F/G:X/Y|Moves],[F1/G1:X1/Y1|Agenda],[F1/G1:X1/Y1|NewAgenda]):-
	F>=F1,
	merge_h([F/G:X/Y|Moves],Agenda,NewAgenda).


/*
 * Grid datastructure: list of H rows of length W. 
 * A variable indicates cell is empty, so it can be 
 * instantiated once with 'x' to indicate an obstacle
 * or with a number to indicate at which search iteration
 * it was explored.
 */
 
empty_grid(W,H,Grid):-
	length(Grid,H),	% generate a list of variables of length H
	empty_rows(W,Grid).	% and instantiate each variable to an empty row

empty_rows(_,[]).
empty_rows(W,[Row|Rows]):-
	length(Row,W),	% generate a list of variables of length W
	empty_rows(W,Rows).

get_row(N,Grid,Row):-
	nth1(N,Grid,Row).

get_col(_,[],[]).
get_col(N,[Row|Rows],[X|Col]):-
	nth1(N,Row,X),
	get_col(N,Rows,Col).

% Create horizontal/vertical obstacles at position X/Y with length L
setx_hor(X/Y,L,Grid):-
	get_row(Y,Grid,Row),
	setx(X,L,Row).

setx_ver(X/Y,L,Grid):-
	get_col(X,Grid,Col),
	setx(Y,L,Col).

setx(_,0,_).
setx(X,L,Row):-
	L>0,L1 is L-1,X1 is X+1,
	nth1(X,Row,x),
	setx(X1,L1,Row).

% Set cell X/Y to N; fails if cell has been set already
set(X/Y,Grid,N):-
	get_row(Y,Grid,Row),
	nth1(X,Row,C),
	var(C),
	C=N.

show_grid(Grid):-
	nl,
	show_rows(Grid),
	nl.

show_rows([]).
show_rows([Row|Rows]):-
	show_row(Row),
	show_rows(Rows).

show_row([]):-nl.
show_row([H|T]):-
	( var(H)    -> format('~|~` t~w~3+ ',['.'])
	; H=x       -> format('~|~` t~w~3+ ',[H])
	; otherwise -> format('~|~` t~d~3+ ',[H])
	),show_row(T). 


/** &lt;examples&gt;

?- member(SS,[df,bf,as,bt]),path(SS).

*/
