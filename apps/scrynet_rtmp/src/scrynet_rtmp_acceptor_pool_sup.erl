-module(scrynet_rtmp_acceptor_pool_sup).
-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, start_acceptor_pool/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    ?LOG_DEBUG("Acceptor pool supervisor started"),
    SupervisorSpecification = #{
        strategy => simple_one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => acceptor_pool,
            start => {scrynet_rtmp_acceptor_pool, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => infinity,
            type => supervisor % worker | supervisor
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.

-spec start_acceptor_pool(Port :: non_neg_integer(), HandlerMod :: atom()) -> supervisor:startchild_ret().
start_acceptor_pool(Port, HandlerMod) ->
    supervisor:start_child(?MODULE, [Port, HandlerMod]).