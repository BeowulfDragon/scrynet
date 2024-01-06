%%%-------------------------------------------------------------------
%% @doc scrynet RTMP ingester
%% Accepts RTMP connections
%% @end
%%%-------------------------------------------------------------------

-module(scrynet_rtmp_acceptor).
-behaviour(gen_server).

-record(listen, {socket :: inet:socket()}).
-record(handshake, {socket :: inet:socket()}).

-include("handshake_info.hrl").

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

start_link(Socket) ->
    gen_server:start_link(?MODULE, {Socket}, []).

init({Socket}) ->
    gen_server:cast(self(), accept),
    {ok, {accept, #listen{socket = Socket}}}.

handle_cast(accept, #listen{socket = ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    scrynet_rtmp_acceptor_sup:create_acceptor(),
    gen_server:cast(self(), handshake),
    {ok, #handshake{socket = AcceptSocket}};

handle_cast(handshake, #handshake{socket = AcceptSocket}) ->
    %%% RTMP handshake (5.2)
    %% C0 and S0 (5.2.2)
    % C0
    {ok, <<3:1/integer-unit:8>>} = gen_tcp:recv(AcceptSocket, 1, 60000),
    % S0
    ok = gen_tcp:send(AcceptSocket, <<3:8/integer>>),
    %% C1 and S1 (5.2.3)
    % S1
    SRandomData = rand:bytes(1528),
    SEpoch = erlang:system_time(millisecond), % IMPORTANT: Monotonic time is different per instance of the runtime system
    ok = gen_tcp:send(AcceptSocket, <<
        0:4/integer-unit:8,
        0:4/integer-unit:8,
        SRandomData:1528/binary
        >>),
    % Version Sent
    % C1
    C1ReadTime = erlang:system_time(millisecond) - SEpoch,
    {ok, <<
        CTimestamp:4/integer-unit:8,
        0:4/integer-unit:8,
        CRandomData:1528/binary
        >>} = gen_tcp:recv(AcceptSocket, 1536, 60000),
    %% C2 and S2 (5.2.4)
    % S2
    ok = gen_tcp:send(AcceptSocket, <<
        CTimestamp:4/integer-unit:8,
        C1ReadTime:4/integer-unit:8,
        CRandomData:1528/binary
        >>),
    % Ack Sent
    % C2
    {ok, <<
        0:4/integer-unit:8,
        _S1ReadTime:4/integer-unit:8,
        SRandomData:1528/binary
        >>} = gen_tcp:recv(AcceptSocket, 1536, 60000),
    % Handshake done
    scrynet_rtmp_handler:start_and_transfer(#handshake_info{socket = AcceptSocket,
                                                                server_epoch = SEpoch,
                                                                client_epoch = CTimestamp}),
    {stop, normal, {}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_info(_info, State) ->
    {noreply, State}.
    
terminate(_Reason, #handshake{socket = AcceptSocket}) ->
    gen_tcp:close(AcceptSocket),
    ok;

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


