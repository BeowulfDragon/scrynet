%%%-------------------------------------------------------------------
%% @doc RTMP Connection Acceptor.
%% Accepts RTMP connections, performs a handshake, and then hands the connection off to a handler.
%% @end
%%%-------------------------------------------------------------------

-module(scrynet_rtmp_acceptor).
-behaviour(gen_server).

-record(state, {socket :: inet:socket(), handler_mod :: atom(), acceptor_sup :: pid(), phase :: accept | handshake}).

-include("handshake_info.hrl").
-include_lib("kernel/include/logger.hrl").

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/3]).

start_link(Socket, HandlerMod, AcceptorSup) ->
    gen_server:start_link(?MODULE, {Socket, HandlerMod, AcceptorSup}, []).

init({Socket, HandlerMod, AcceptorSup}) ->
    ?LOG_DEBUG("Listener init"),
    gen_server:cast(self(), accept),
    ?LOG_DEBUG("Listener started"),
    {ok, {accept, #state{socket = Socket, handler_mod = HandlerMod, acceptor_sup = AcceptorSup, phase = accept}}}.

handle_cast(accept, #state{socket = ListenSocket, acceptor_sup = AcceptorSup} = State) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    ?LOG_DEBUG("Acceptor recieved connection"),
    scrynet_rtmp_acceptor_pool:create_acceptor(AcceptorSup),
    gen_server:cast(self(), handshake),
    {ok, State#state{socket = AcceptSocket, phase = handshake}};

handle_cast(handshake, #state{socket = AcceptSocket, handler_mod = HandlerMod}) ->
    %%% RTMP handshake (5.2)
    %% C0 and S0 (5.2.2)
    % C0
    {ok, <<3:1/integer-unit:8>>} = gen_tcp:recv(AcceptSocket, 1, 60000),
    ?LOG_DEBUG("C0 recieved"),
    % S0
    ok = gen_tcp:send(AcceptSocket, <<3:8/integer>>),
    ?LOG_DEBUG("S0 sent"),
    %% C1 and S1 (5.2.3)
    % S1
    SRandomData = rand:bytes(1528),
    SEpoch = erlang:system_time(millisecond), % IMPORTANT: Monotonic time is different per instance of the runtime system
    ok = gen_tcp:send(AcceptSocket, <<
        0:4/integer-unit:8,
        0:4/integer-unit:8,
        SRandomData:1528/binary
        >>),
    ?LOG_DEBUG("S1 sent"),
    % Version Sent
    % C1
    C1ReadTime = erlang:system_time(millisecond) - SEpoch,
    {ok, <<
        CTimestamp:4/integer-unit:8,
        0:4/integer-unit:8,
        CRandomData:1528/binary
        >>} = gen_tcp:recv(AcceptSocket, 1536, 60000),
    ?LOG_DEBUG("C1 recieved"),
    %% C2 and S2 (5.2.4)
    % S2
    ok = gen_tcp:send(AcceptSocket, <<
        CTimestamp:4/integer-unit:8,
        C1ReadTime:4/integer-unit:8,
        CRandomData:1528/binary
        >>),
    ?LOG_DEBUG("S2 sent"),
    % Ack Sent
    % C2
    {ok, <<
        0:4/integer-unit:8,
        _S1ReadTime:4/integer-unit:8,
        SRandomData:1528/binary
        >>} = gen_tcp:recv(AcceptSocket, 1536, 60000),
    ?LOG_DEBUG("C2 recieved"),
    % Handshake done
    scrynet_rtmp_connection:start_and_transfer(#handshake_info{socket = AcceptSocket,
                                                                server_epoch = SEpoch,
                                                                client_epoch = CTimestamp},
                                                HandlerMod),
    {stop, normal, {}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_info(_info, State) ->
    {noreply, State}.
    
terminate(_Reason, #state{socket = AcceptSocket, phase = handshake}) ->
    gen_tcp:close(AcceptSocket),
    ok;

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


