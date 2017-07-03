-module(lfunc).
-export([product_d/1, product_t/1, maximum_d/1, maximum_t/1]).

% empty list should get the "zero-element" for product.
product_d([]) ->
	1;
product_d([X|Xs]) ->
	X*product_d(Xs).

product_t(Xs) -> product_t(Xs, 1).
product_t([], P) -> P;
product_t([X|Xs], P) -> product_t(Xs, X*P).

maximum_d([X|Xs]) ->
	case Xs of
		[] -> X;
		Xs -> max(X, maximum_d(Xs))
	end.

maximum_t([X|Xs]) -> maximum_t(Xs, X).
maximum_t([], M)     -> M;
maximum_t([X|Xs], M) -> maximum_t(Xs, max(X,M)).
