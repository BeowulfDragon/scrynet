-module(scrynet_rtmp_connection_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
    SupervisorSpecification = #{
        strategy => simple_one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => connection_handler,
            start => {scrynet_rtmp_handler, start_link, []},
            restart => temporary, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [scrynet_rtmp_handler]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.

start_child(HandshakeInfo, HandlerMod) ->
    supervisor:start_child(?MODULE, [HandshakeInfo, HandlerMod]).

