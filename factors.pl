% factors(N,Fs) <- Fs is the list of prime factors of N
factors(1,[1]).
factors(X,[Factor|Fs]) :-
   X > 1,
   between(2,X,Factor),    % generate candidate factor, lowest first
   (X mod Factor) =:= 0,   % test Factor is divisor of X
   !,                      % if so, stop generating!
   Rem is X // Factor,     % proceed with remainder
   factors(Rem,Fs).

/** &lt;examples&gt;

?-factors(27,Fs).
?-factors(28,Fs).
?-factors(29,Fs).

*/
