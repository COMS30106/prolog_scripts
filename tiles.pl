board_size(7). % odd number, at least 3

eval_pos(Pos,Value):-
	%% uncomment exactly one of the following lines
	bLeftOfw(Pos,Value).    
	%outOfPlace(Pos,1,Value). 

/*
 * Moves are represented as triples m(From,To,Cost).
 * Agenda items are represented as pairs v(Value,[Move|Moves]) 
 * where Value is the heuristic value of Move obtained by ?-eval_move(Move,Value).
  */

% tiles(M,C) <- moves M lead to a goal position at cost C 
%               (best-first search strategy)
tiles(Moves,Cost):-
	start_pos(Start),
	eval_pos(Start,Value),
	tiles_a([v(Value,[m(noparent,Start,0)])],Moves,[],_Visited),
	get_cost(Moves,0,Cost).

% write solution to screen
tiles(Cost):-
	tiles(Moves,Cost),
	writeln('# Final list of moves #'),
	reverse(Moves,MovesR),
	write_moves(MovesR,0).

% tiles_a(A,M,V0,V) <- goal position can be reached from one of the
%                      positions on A with moves M (best-first)
%                      (V is list of visited nodes, V0 is accumulator)
tiles_a([v(_V,[LastMove|Moves])|_Rest],[LastMove|Moves],Visited,Visited):-
	eval_move(LastMove,0). % reached our goal
tiles_a([v(_V,[LastMove|_])|Rest],Moves,Visited0,Visited):-
	eval_move(LastMove,0),!, % to get alternate solutions
	tiles_a(Rest,Moves,Visited0,Visited).
tiles_a(Agenda,Moves,Visited0,Visited):-
	% writeln('### current agenda (reversed) ###'),reverse(Agenda,AgendaR),write_agenda(AgendaR),readln(_), 
	%% uncomment the previous line to see agenda at each iteration
	Agenda=[v(_V,[LastMove|Ms])|Rest],
	( setof(v(Value,[NextMove,LastMove|Ms]),
        candidate(LastMove,NextMove,Value,Rest,Visited0),
        Children) -> new_agenda(Children,Rest,NewAgenda)	% best-first
    ; otherwise   -> NewAgenda=Rest	% candidate/5 failed
	),tiles_a(NewAgenda,Moves,[LastMove|Visited0],Visited).

candidate(LastMove,NextMove,Value,Agenda,Visited):-
	last2next_move(LastMove,NextMove),
	NextMove=m(_Pos,NewPos,_Cost),
	eval_move(NextMove,Value),
	not((member(v(_,[m(_,NewPos,_)|_]),Agenda))), % NewPos already on Agenda
	not((member(m(_,NewPos,_),Visited))).         % NewPos already visited
% The last two lines implement aggressive pruning as only NewPos matters, 
% not the position Pos from which it is reached.

eval_move(m(_P,Pos,_C),Value):-
	eval_pos(Pos,Value).

last2next_move(m(_OldPos,Pos,_OldCost),m(Pos,NewPos,Cost)):-
	last2next_pos(Pos,NewPos,Cost).

% merge children and agenda
new_agenda([],Agenda,Agenda).
new_agenda([C|Cs],[],[C|Cs]).
new_agenda([v(V1,Moves1)|Rest1],[v(V2,Moves2)|Rest2],NewAgenda):-
	( V2>V1     -> new_agenda(Rest1,[v(V2,Moves2)|Rest2],Rest3),NewAgenda=[v(V1,Moves1)|Rest3]
	; otherwise -> new_agenda([v(V1,Moves1)|Rest1],Rest2,Rest3),NewAgenda=[v(V2,Moves2)|Rest3]
	).

get_cost([],C,C).
get_cost([m(_From,_To,C)|Moves],Cost0,Cost):-
	Cost1 is Cost0+C,
	get_cost(Moves,Cost1,Cost).

write_agenda([]):-
	writeln('-------------------').
write_agenda([P|Ps]):-
	write_agenda_item(P),nl,
	write_agenda(Ps).

write_agenda_item(v(Value,[m(_P,Pos,_C)|_Rest])):-
	write_pos(Pos),
	write(' : '),write(Value).

write_moves([],_):-
	writeln('-------------------').
write_moves([m(_P,Pos,C)|Moves],Cost):-
	eval_pos(Pos,Value),
	Cost1 is Cost+C,
	write_pos(Pos),
	format(' : ~|~` t~d~2+ :~|~` t~d~3+~n', [Value,Cost1]),
	write_moves(Moves,Cost1).

%%% board predicates

start_pos(Start):-
	board_size(N),
	start_pos(Start,N).

start_pos(Start,N):-
	N1 is (N-1)/2,
	start_lr(Left,N1,b),
	start_lr(Right,N1,w),
	append(Left,[e|Right],Start).

start_lr(LR,N,BW):-
	length(LR,N),
	start_lr(LR,BW).

start_lr([],_).
start_lr([BW|LR],BW):-
	start_lr(LR,BW).

last2next_pos(Pos,NewPos,Cost):-
	get_tile(Pos,Ne,e),get_tile(Pos,Nbw,BW),not(BW=e),
	Diff is abs(Ne-Nbw),
	Diff<4,
	replace(Pos,Ne,BW,Pos1),
	replace(Pos1,Nbw,e,NewPos),
	( Diff=1    -> Cost=1
	; otherwise -> Cost is Diff-1 ).

write_pos([]).
write_pos([P|Ps]):-
	( P=e -> write('. ')
	; P=b -> write('\x25cf'),write(' ')
	; P=w -> write('\x25cb'),write(' ')
	),write_pos(Ps).

get_tile(Pos,N,S):-
	get_tile(Pos,1,N,S).

get_tile([X|_Xs],N,N,X).
get_tile([_X|Xs],N0,N,Y):-
	N1 is N0+1,
	get_tile(Xs,N1,N,Y).

%%% heuristics

bLeftOfw(Pos,Value):-
	findall(b,(get_tile(Pos,Nb,b),get_tile(Pos,Nw,w),Nw>Nb),L),
	length(L,Value).

outOfPlace(Pos,Initial,Value):-
	append(L,[_Mid|Right],Pos),  % generate
	length(L,N),length(Right,N), % test
	reverse(L,Left), % from low to high penalty
	penalise(Left,b,Initial,0,VL),
	penalise(Right,w,Initial,0,VR),
	Value is VL+VR. 

penalise([],_,_,V,V).
penalise([H|T],BW,P,V0,V):-
	( H=BW -> V1 is V0+P
	; otherwise -> V1=V0
	),
	P1 is P+1, 
	penalise(T,BW,P1,V1,V).

%%% auxiliary predicates

% replace(In,Pos,Item,Out) <- list Out is List in with item at Pos replaced with Item
replace([_X|Xs],1,Y,[Y|Xs]).
replace([X|Xs],N,Y,[X|Zs]):-
	N>1,N1 is N-1,
	replace(Xs,N1,Y,Zs).

/** &lt;examples&gt;

?- tiles(Moves,Cost).
?- tiles(Cost). % prints agenda

*/
