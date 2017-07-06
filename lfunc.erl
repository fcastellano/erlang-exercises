-module(lfunc).
-export([product_d/1,product_t/1,maximum_d/1,maximum_t/1,maximum/1,double/1,even/1,median/1,modes/1,tests/1]).

even([]) -> [];
even([X|Xs]) when X rem 2 == 0 -> [X|even(Xs)];
even([X|Xs]) when X rem 2 == 1 -> even(Xs).
%even([X|Xs]) ->
	%case X rem 2 of
		%0 -> [X|even(Xs)];
		%1 -> even(Xs)
	%end.

double([]) -> [];
double([X|Xs]) -> [2*X|double(Xs)].

% empty list should get the "zero-element" for product.
product_d([]) ->
	1;
product_d([X|Xs]) ->
	X*product_d(Xs).

product_t(Xs) -> product_t(Xs, 1).
product_t([], P) -> P;
product_t([X|Xs], P) -> product_t(Xs, X*P).

maximum([X]) -> X;
maximum([X|Xs]) -> max(X, maximum(Xs)).

maximum_d([X|Xs]) ->
	case Xs of
		[] -> X;
		Xs -> max(X, maximum_d(Xs))
	end.

maximum_t([X|Xs]) -> maximum_t(Xs, X).
maximum_t([], M)     -> M;
maximum_t([X|Xs], M) -> maximum_t(Xs, max(X,M)).

%%%% Modes and Median with helper functions
% Given a list X, if any element occurs just once, then there is no mode!
% Otherwise, there exists a maximum number of recurrences of a certain value;
% then the modes are all the values that occur such a maximum number of times!
modes([]) -> []; % no value, no mode
modes(X) ->
	R = recurrences(X), % recurrence list
	{C, _} = maximum(R),
	case C of
		1 -> [];
		_ -> rec_filter(R, C)
	end.

median([X]) -> X;
median(X) ->
	C=count(X),
	S=sort(X),
	Mi=C div 2, % median 0-based index of the sorted list
	case C rem 2 of
		0 -> [L,U] = extract(S, [Mi - 1, Mi]),
		     (L+U)/2;
		1 -> [M] = extract(S, [Mi]),
		     M
	end.

%%%%%%%%%%%%%%%
%%%% TOOLS %%%%
%%%%%%%%%%%%%%%
% non-empty list calls a 2-arg function, the first with the ordered list, the
% second, with the list to be sorted (that contains at lease 3 elements!!!)
sort([]) -> [];
sort([X|Xs]) -> sort([], [X|Xs]).

% Continue pouring leftward sorted elements, but when a swap occurs, let's
% start anew
sort(S, [X,Y|Us]) when X=<Y -> sort(S ++ [X], [Y|Us]);
sort(S, [X,Y|Us]) when X>Y  -> sort([],       S++[Y,X|Us]);
% If I left just an elements, let's return the sorted list
sort(S, [X])                -> S++[X].

% count the elements of a list
count([]) -> 0;
count([_X|Xs]) -> 1+count(Xs).

% given a list X, and a indices list I, I'll walk X via tail recursion,
% building a list of X elements with index in I
extract(X, I) -> extract(X, I, [], 0, I).

% I walked all the input list, let's return what found
extract([],     _I, R, _Cx, _J)                   -> R;
% I walked all the indexes list, let's evaluate the next element in the input list
extract([_X|Xs],  I, R,  Cx, [])                  -> extract(Xs,     I, R,        Cx+1, I);
% Current index (J) is equal to the index of the currently evaluated input list (Cx)
% element; keep it, next element in the input list, restore the index list
extract([X|Xs],  I, R,  Cx, [J|_Js]) when J == Cx -> extract(Xs,     I, R ++ [X], Cx+1, I);
% Otherwise, keep walking the next indexes element
extract([X|Xs],  I, R,  Cx, [J|Js]) when J =/= Cx -> extract([X|Xs], I, R,        Cx,   Js).

%% recurrences(X): given a list X:
%% 1. sort X
%% 2. re-represent X into [{Occurrences_V1, V1}, {Occurrences_V2, V2}, ...]
recurrences(X) -> recurrences(sort(X), []).

recurrences([S|Ss], [])          -> recurrences(Ss, [{1, S}]);
recurrences([S|Ss], [{O, S}|Rs]) -> recurrences(Ss, [{O+1, S}|Rs]);
recurrences([S|Ss], Rs)          -> recurrences(Ss, [{1, S}|Rs]);
recurrences([], R)               -> R.

%% filter(recurrence_list, C), gives, the values, with C occurrences, in the
%% recurrence_list
rec_filter([], _C) -> [];
rec_filter([{C, V}|Rs], C) -> [V|rec_filter(Rs, C)];
rec_filter([{_O, _V}|Rs], C) -> rec_filter(Rs, C).

%%%%%%%%%%%%%%%
%%%% Tests %%%%
%%%%%%%%%%%%%%%
tests(sort) ->
	[
	 {"[] == sort([])",           [] == sort([])},
	 {"[1] == sort([1])",         [1] == sort([1])},
	 {"[1,1] == sort([1,1])",     [1,1] == sort([1,1])},
	 {"[1,2] == sort([1,2])",     [1,2] == sort([1,2])},
	 {"[1,2] == sort([2,1])",     [1,2] == sort([2,1])},
	 {"[1,2,3] == sort([1,2,3])", [1,2,3] == sort([1,2,3])},
	 {"[1,2,3] == sort([1,3,2])", [1,2,3] == sort([1,3,2])},
	 {"[1,2,3] == sort([3,1,2])", [1,2,3] == sort([3,1,2])},
	 {"[1,2,3] == sort([3,2,1])", [1,2,3] == sort([3,2,1])},
	 {"[2,3,3] == sort([3,3,2])", [2,3,3] == sort([3,3,2])},
	 {}
	];
tests(even) ->
	[
	 {"[] == even([1])",     [] == even([1])},
	 {"[] == even([1,1])", [] == even([1,1])},
	 {"[2] == even([1,2])", [2] == even([1,2])},
	 {"[2] == even([2,1])", [2] == even([2,1])},
	 {"[2] == even([1,2,3])", [2] == even([1,2,3])},
	 {"[2,2] == even([2,3,2])", [2,2] == even([2,3,2])},
	 {"[4,2] == even([3,4,2])", [4,2] == even([3,4,2])},
	 {"[] == even([3,5,1])", [] == even([3,5,1])},
	 {"[0,2,4] == even([0,3,3,2,4])", [0,2,4] == even([0,3,3,2,4])},
	 {}
	];
tests(double) ->
	[
	 {"[2] == double([1])",   [2] == double([1])},
	 {"[2,2] == double([1,1])", [2,2] == double([1,1])},
	 {"[2,4] == double([1,2])", [2,4] == double([1,2])},
	 {"[4,2] == double([2,1])", [4,2] == double([2,1])},
	 {"[2,4,6] == double([1,2,3])", [2,4,6] == double([1,2,3])},
	 {"[2,6,4] == double([1,3,2])", [2,6,4] == double([1,3,2])},
	 {"[6,2,4] == double([3,1,2])", [6,2,4] == double([3,1,2])},
	 {"[6,4,2] == double([3,2,1])", [6,4,2] == double([3,2,1])},
	 {"[6,6,4] == double([3,3,2])", [6,6,4] == double([3,3,2])},
	 {}
	];
tests(median) ->
	[
	 {"1 == median([1])",     1 == median([1])},
	 {"1.0 == median([1,1])", 1.0 == median([1,1])},
	 {"1.5 == median([1,2])", 1.5 == median([1,2])},
	 {"1.5 == median([2,1])", 1.5 == median([2,1])},
	 {"2 == median([1,2,3])", 2 == median([1,2,3])},
	 {"2 == median([1,3,2])", 2 == median([1,3,2])},
	 {"2 == median([3,1,2])", 2 == median([3,1,2])},
	 {"2 == median([3,2,1])", 2 == median([3,2,1])},
	 {"3 == median([3,3,2])", 3 == median([3,3,2])},
	 {}
	];
tests(modes) ->
	[
	 {"[] == modes([])",       []  == modes([])},
	 {"[1] == modes([1,1])",   [1] == modes([1,1])},
	 {"[] == modes([1,2])",    []  == modes([1,2])},
	 {"[] == modes([2,1])",    []  == modes([2,1])},
	 {"[] == modes([1,2,3])",  []  == modes([1,2,3])},
	 {"[3] == modes([3,3,2])", [3] == modes([3,3,2])},
	 % modes is not stable with sorting values, so, let's sort before comparing!
	 {"[3,2] == modes([3,3,2,2])", [2,3] == sort(modes([3,3,2,2]))},
	 {"[3,2] == modes([3,3,2,2,1])", [2,3] == sort(modes([3,3,2,2,1]))},
	 {}
	];
tests(all) ->
	tests(modes) ++
	tests(median) ++
	tests(double) ++
	tests(even) ++
	tests(sort).

