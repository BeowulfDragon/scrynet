%%%-------------------------------------------------------------------
%% @doc scrynet public API
%% @end
%%%-------------------------------------------------------------------

-module(scrynet_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    scrynet_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
