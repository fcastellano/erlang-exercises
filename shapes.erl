-module(shapes).
-export([perimeter/1,area/1,enclose/1,tests/0]).

% Shapes
% ------
% {circle, {X_centre, Y_centre}, Radius}
% {rectangle, {X_lower_left, Y_lower_left}, {X_upper_right, Y_upper_right}}
% {triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}

% At first I chose a representation of rectangle based on its lower left and
% upper right vertices, but seeing again the video, I feared an alternative
% representation should be used:
% {rectangle, {X_centre, Y_centre}, H, W}
% So let's add handling for this representation too, and a utility function to
% convert between them.

max3(A,B,C) ->
	max(A,max(B,C)).
min3(A,B,C) ->
	min(A,min(B,C)).

enclose({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}) ->
	{rectangle, {min3(X1,X2,X3), min3(Y1,Y2,Y3)}, {max3(X1,X2,X3), max3(Y1,Y2,Y3)}};
enclose({rectangle, {X1, Y1}, {X2, Y2}}) ->
	{rectangle, {X1, Y1}, {X2, Y2}};
enclose({circle, {X, Y}, R}) ->
	{rectangle, {X-R, Y-R}, {X+R, Y+R}}.

% convert representation of rectangle
convert({rectangle, {X1, Y1}, {X2, Y2}}) ->
	X=(X1+X2)/2,
	Y=(Y1+Y2)/2,
	W=abs(X2-X1),
	H=abs(Y2-Y1),
	{rectangle, {X, Y}, H, W};
convert({rectangle, {X, Y}, H, W}) ->
	X1=X-W/2,
	X2=X+W/2,
	Y1=Y-W/2,
	Y2=Y+H/2,
	{rectangle, {X1,Y1}, {X2,Y2}}.

% Take advantage of Heron's Formula
area({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}) ->
	S=perimeter({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}})/2,
	A=math:sqrt((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)),
	B=math:sqrt((X3-X2)*(X3-X2)+(Y3-Y2)*(Y3-Y2)),
	C=math:sqrt((X1-X3)*(X1-X3)+(Y1-Y3)*(Y1-Y3)),
	math:sqrt(S*(S-A)*(S-B)*(S-C));
area({rectangle, {_X, _Y}, H, W}) ->
	H*W;
area({rectangle, {X1, Y1}, {X2, Y2}}) ->
	%abs(X2-X1)*abs(Y2-Y1);
	area(convert({rectangle, {X1, Y1}, {X2, Y2}}));
area({circle, {_X, _Y}, R}) ->
	math:pi()*R*R.

perimeter({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}) ->
	math:sqrt((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1))+
	math:sqrt((X3-X2)*(X3-X2)+(Y3-Y2)*(Y3-Y2))+
	math:sqrt((X1-X3)*(X1-X3)+(Y1-Y3)*(Y1-Y3));
perimeter({rectangle, {_X, _Y}, H, W}) ->
	2*(H+W);
perimeter({rectangle, {X1, Y1}, {X2, Y2}}) ->
	%2*(abs(X2-X1)+abs(Y2-Y1));
	perimeter(convert({rectangle, {X1, Y1}, {X2, Y2}}));
perimeter({circle, {_X, _Y}, R}) ->
	2*math:pi()*R.

tests() ->
	[
	 {
	  'convert idempotence',
	  convert(convert({rectangle, {0,0}, 2, 2})) == {rectangle, {0,0}, 2, 2}
	 },
	 {
	  '1-side square area is 1',
	  area({rectangle, {0, 0}, 1, 1}) == 1
	 },
	 {
	  '3-4-5 right triangle area is 6',
	  area({triangle, {0,0}, {3,0}, {0,4}}) == 6
	 },
	 {
	  '3-4-5 right triangle perimeter is 12',
	  perimeter({triangle, {0,0}, {3,0}, {0,4}}) == 12
	 },
	 {
	  '3-4-5 right triangle perimeter is enclosed in a 12 area rectangle',
	  area(enclose({triangle, {0,0}, {3,0}, {0,4}})) == 12
	 },
	 {
	  '1/2 redius circle has PI perimeter',
	  perimeter({circle, {10,10}, 0.5}) == math:pi()
	 },
	 {
	  'unit circle has PI area',
	  area({circle, {-10,-10}, 1.0}) == math:pi()
	 }
	].
