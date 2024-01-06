%%%-------------------------------------------------------------------
%% @doc RTMP ingestion acceptor supervisor
%% Creates 20 RTMP acceptors and exposes a function to create new ones.
%% Each acceptor should create a new acceptor upon reciving a connection.
%% @end
%%%-------------------------------------------------------------------
-module(scrynet_rtmp_acceptor_sup).
-behaviour(supervisor).

%% API
-export([start_link/1, create_acceptor/0]).
-export([init/1]).

start_link(OnConnect) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, OnConnect).

init(_Args) ->
    {ok, ListenSocket} = gen_tcp:listen(1935, [{packet, raw}]),
    SupervisorSpecification = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => acceptor,
            start => {scrynet_rtmp_acceptor, start_link, [ListenSocket]},
            restart => temporary,
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [scrynet_rtmp_acceptor]
        }
    ],

    spawn_link(fun () -> empty_acceptors(20) end),

    {ok, {SupervisorSpecification, ChildSpecifications}}.

create_acceptor() ->
    supervisor:start_child(?MODULE, []).

-spec empty_acceptors(Count :: pos_integer()) -> ok.
empty_acceptors(Count) ->
    [create_acceptor() || _ <- lists:seq(1,Count)],
    ok.