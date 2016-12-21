:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(www_browser)).

% wp(Q,T) <- issue query Q to Wikipedia and return the page in wikitext format
wp(Q,T):-
	query2URL(Q,URL),
	http_get(URL,R,[]),
	atom_json_term(R,RR,[]),
	get_wikitext(RR,T),!.

% wp(Q) <- issue query Q to Wikipedia and write the page in wikitext format to stdout
wp(Q):-
	wp(Q,T),
	atomic_list_concat(L,'\n',T),	% split text T into segments separated by newlines
	writelns(L).	% write the list of segments

% assemble query Q into a URL to retrieve the page in JSON format
query2URL(Q,URL):-
	atomic_list_concat(QL,' ',Q),	% split query Q into words QW
	atomic_list_concat(QL,'%20',QQ),	% reassemble query QQ with '%20' between words from QW
	atomic_list_concat([
		'http://en.wikipedia.org/w/api.php?format=json&action=query&titles=',
		QQ,
		'&prop=revisions&rvprop=content'
		],URL). 

% decompose JSON Prolog term T until wikitext component is located
get_wikitext(J,Text):-
	( J = ('*'=Text) -> true
	; J = (json(L)) -> get_wikitext(L,Text)
	; J = (_=json(L)) -> get_wikitext(L,Text)
	; J = (_=L) -> get_wikitext(L,Text)
	; J = [H|T] -> ( get_wikitext(H,Text) ; get_wikitext(T,Text) )
	).

% write a list of items with a newline after each
writelns([]):-nl.
writelns([H|T]):-
	write(H),nl,
	writelns(T).
