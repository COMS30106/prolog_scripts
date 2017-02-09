% powerset(S,PS) <- PS is the list of all sublists of S
% First try
powerset1([],[[]]).
powerset1([H|T],PowerSet):-
    powerset1(T,PowerSetOfT),
    extend_pset(H,PowerSetOfT,PowerSet). % not tail-recursive

% extend_pset(X,S,ES) <- ES contains two copies of each list in S, 
%                        one with X and one without
extend_pset(_,[],[]).
extend_pset(H,[List|MoreLists],[List,[H|List]|More]):-
    extend_pset(H,MoreLists,More).
    
% Second try: swap calls in recursive clause
powerset2([],[[]]).
powerset2([H|T],PowerSet):-
    extend_pset(H,PowerSetOfT,PowerSet), % generator (bad! why?)
    powerset2(T,PowerSetOfT).            % tail-recursive

% Third try: use accumulator
powerset([],PowerSet,PowerSet).
powerset([H|T],Acc,PowerSet):-
    extend_pset(H,Acc,Acc1),   % efficient generator
    powerset(T,Acc1,PowerSet). % tail-recursive

powerset(Set,PowerSet):-powerset(Set,[[]],PowerSet).

/** &lt;examples&gt;

?- powerset1([a,b,c,d,e,f],PS).
?- powerset2([a,b,c,d,e,f],PS).
?- powerset([a,b,c,d,e,f],PS).
?- extend_pset(1,[[],[2],[3],[2,3]],ES).
?- extend_pset(1,S,ES).

*/
