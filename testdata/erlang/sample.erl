-module(sample).
-export([start/0, greet/1]).

-include("records.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Start the application.
start() ->
    io:format("Starting~n").

%% Greet a user.
greet(Name) ->
    io:format("Hello, ~s~n", [Name]).

%% Internal helper (not exported).
helper(X) ->
    X * 2.
