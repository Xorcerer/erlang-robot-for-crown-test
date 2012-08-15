%% Author: Jackie
%% Created: 2012-8-14
%% Description: TODO: Add description to math_util
-module(math_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export(
	[
		add_vec2d/2,
		sub_vec2d/2,
		scale_vec2d/2,
		norm_vec2d/1,
		len_squared_vec2d/1,
		len_vec2d/1,
		dist_squared_vec2d/2,
		dist_vec2d/2
	]).

%%
%% API Functions
%%
%% vector
add_vec2d({X1, Y1}, {X2, Y2}) ->
	{X1 + X2, Y1 + Y2}.

sub_vec2d({X1, Y1}, {X2, Y2}) ->
	{X1 - X2, Y1 - Y2}.

scale_vec2d({X, Y}, S) ->
	{X * S, Y * S}.

norm_vec2d(Vec) ->
	Len = len_vec2d(Vec),
	scale_vec2d(Vec, 1.0/Len).

len_squared_vec2d({X, Y}) ->
	X * X + Y * Y.

len_vec2d(Vec) ->
	math:sqrt(len_squared_vec2d(Vec)).

dist_squared_vec2d(Vec1, Vec2) ->
	len_squared_vec2d(sub_vec2d(Vec1, Vec2)).

dist_vec2d(Vec1, Vec2) ->
	math:sqrt(dist_squared_vec2d(Vec1, Vec2)).

%%
%% Local Functions
%%

