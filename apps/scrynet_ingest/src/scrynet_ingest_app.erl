-module(scrynet_ingest_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    scrynet_ingest_sup:start_link().

stop(_State) ->
    ok.