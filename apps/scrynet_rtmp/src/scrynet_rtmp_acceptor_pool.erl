%%%-------------------------------------------------------------------
%% @doc RTMP Connection Acceptor Pool.
%% Creates 20 RTMP acceptors and exposes a function to create new ones.
%% Each acceptor should create a new acceptor upon reciving a connection.
%% @end
%%%-------------------------------------------------------------------
-module(scrynet_rtmp_acceptor_pool).
-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").
%% API
-export([start_link/2, create_acceptor/1]).
-export([init/1]).

start_link(Port, HandlerMod) ->
    supervisor:start_link(?MODULE, {Port, HandlerMod}).

init({Port, HandlerMod}) ->
    ?LOG_DEBUG("Acceptor pool starting on port ~w", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{packet, raw}]), %% todo: check if this will close
    ?LOG_DEBUG("Acceptor pool listening on port ~w", [Port]),
    SupervisorSpecification = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => acceptor,
            start => {scrynet_rtmp_acceptor, start_link, [ListenSocket, HandlerMod, self()]},
            restart => temporary,
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [scrynet_rtmp_acceptor]
        }
    ],

    spawn_link(fun () -> empty_acceptors(20, self()) end),

    {ok, {SupervisorSpecification, ChildSpecifications}}.

create_acceptor(PID) ->
    supervisor:start_child(PID, []).

-spec empty_acceptors(Count :: pos_integer(), PID :: pid()) -> ok.
empty_acceptors(Count, PID) ->
    [create_acceptor(PID) || _ <- lists:seq(1,Count)],
    ok.