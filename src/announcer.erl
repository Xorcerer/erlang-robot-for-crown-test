%% Author: Jackie
%% Created: 2012-9-11
%% Description: TODO: Add description to announcer
-module(announcer).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([announce/0]).

%%
%% API Functions
%%
announce() ->
	receive
		{From, request} ->
			From ! {self(), response, get()};
		{_, report, UserId, State} ->
			put(UserId, State)
	end.

%%
%% Local Functions
%%

