-module(bits).
-export([bits_dr/1, bits_tr/1, bits/1, tests/0]).

% plain recursive
bits_dr(0) ->
	0;
bits_dr(N) when N>0 ->
	N rem 2 + bits_dr(N div 2).

% Tail recursive, i.e., looping
bits_tr(0,S) ->
	S;
bits_tr(N,S) when N>0 ->
	bits_tr(N div 2,S+N rem 2). % yes, 2+3 rem 2 == 3

bits_tr(N) ->
	bits_tr(N,0).

% Let's try a complex return value
% be aware of clauses order!
bits(0) ->
	0;
bits({0,S}) when S>0 ->
	S;
bits({N,S}) ->
	bits({N div 2, S+N rem 2}); % yes, 2+3 rem 2 == 3
bits(N) ->
	bits({N, 0}).

tests() ->
	% test vector
	Y=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],
	% they should have a similar result, isn't it?
	[{'input', 'with tuples', 'direct', 'tail', 'agree?'}, % header
	 lists:map(
	  fun (X) -> {
		 X,		% input value
		 bits(X),	% tail recursive with tuple
		 bits_dr(X),	% direct recursive
		 bits_tr(X),	% tail recursive
		 (bits(X) == bits_dr(X)) and (bits_dr(X) == bits_tr(X))
		 		% do they agree?
		} end,
	  Y
	 )].
