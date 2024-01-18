%%%-------------------------------------------------------------------
%% @doc Top-level RTMP server application supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(scrynet_rtmp_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_all, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => connections,
            start => {scrynet_rtmp_connection_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor
        },
        #{
            id => acceptor_pools,
            start => {scrynet_rtmp_acceptor_pool_sup, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => infinity,
            type => supervisor % worker | supervisor
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.
