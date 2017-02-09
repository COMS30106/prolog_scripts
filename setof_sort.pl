:-use_module(library(random)). % for randseq/3 to generate random lists

setof_sort(In,Out):-
    setof(X,member(X,In),Out).

/** &lt;examples&gt;

?- randseq(20,100,In), setof_sort(In,Out).

*/
