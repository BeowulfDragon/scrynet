%%%-------------------------------------------------------------------
%% @doc RTMP Connection Handler.
%% Takes over a connection once the RTMP handshake is performed.
%% Handles the connection and sends completed chunks to a some other process.
%% @end
%%%-------------------------------------------------------------------
-module(scrynet_rtmp_connection).
-behaviour(gen_server).

-include("handshake_info.hrl").

-export([start_and_transfer/2, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export_type([connection_worker_ref/0]).

-record(chunk_stream, {
            length :: integer(),
            type :: integer(),
            stream_id :: integer(),
            recieved_data :: binary(),
            time :: integer(),
            extended_timestamp :: boolean()
    }).

-record(state, {socket :: gen_tcp:socket(),
                ownership = false :: boolean(),
                chunk_streams = #{} :: #{integer() := #chunk_stream{}},
                server_epoch :: integer(),
                client_epoch :: integer(),
                send_max_chunk_size = 128 :: non_neg_integer(),
                recieve_max_chunk_size = 128 :: non_neg_integer(),
                recieve_window_acknowledgement_size = 0 :: non_neg_integer(),
                recieved_sequence = 0 :: non_neg_integer(),
                recieved_since_last_ack = 0 :: non_neg_integer(),
                recieve_last_limit = soft :: soft | hard, % Last limit type set for the peer by here
                send_window_acknowledgement_size = 0 :: integer(),
                sent_sequence = 0,
                sent_since_last_ack = 0,
                send_last_limit = soft :: soft | hard, % Last limit type set by the peer for here
                handler_module :: atom(),
                handler_state = {} :: term()}).

-record(basic_header, {format :: 0 | 1 | 2 | 3, csid :: 0..65599}).
-type basic_header() :: #basic_header{}.

-type chunk_header() :: #{time := integer(), length => 0..16777215, type => 0..255, stream_id => 0..4294967295}.

-opaque connection_worker_ref() :: {pid()}.

%%% API

%% @doc Set the bandwidth limit with a reference to a worker.
%% For use outside the module.
%% @param Ref Reference to the worker process.
%% @param Size The bandwidth limit to set in bytes.
%% @param Type The type of bandwidth limit to set.
%% @returns The atom ok, as this is a wrapper around a cast.
-spec set_peer_bandwidth(Ref :: connection_worker_ref(), Size :: non_neg_integer(), Type :: soft | hard | dynamic) -> ok.
set_peer_bandwidth(Ref, Size, Type) ->
    Pid = get_worker_pid(Ref),
    gen_server:cast(Pid, {set_peer_bandwidth, Size, Type}).

%% @doc Start a connection worker and transfer owner of the socket to it.
%% Just localizes this part here.
%% @param HandshakeInfo The information pertaining to the handshake process.
%% @param HandlerMod The module that contains functions for handling the connection.
%% @returns a supervisor:startchild_ret().
-spec start_and_transfer(HandshakeInfo :: #handshake_info{}, HandlerMod :: atom()) -> supervisor:startchild_ret().
start_and_transfer(HandshakeInfo, HandlerMod) ->
    Ret = scrynet_rtmp_connection_sup:start_child(HandshakeInfo, HandlerMod),
    case Ret of 
        {ok, Child} when is_pid(Child) -> % Guard is not really needed
            ok = gen_tcp:controlling_process(HandshakeInfo#handshake_info.socket, Child),
            gen_server:cast(Child, ownership),
            Ret;
        {ok, Child, _Info} when is_pid(Child) ->
            ok = gen_tcp:controlling_process(HandshakeInfo#handshake_info.socket, Child),
            gen_server:cast(Child, ownership),
            Ret
    end.

%%% Internal

start_link(HandlerMod, HandshakeInfo) ->
    gen_server:start_link(?MODULE, {HandlerMod, HandshakeInfo}, []).

init({HandlerMod, HandshakeInfo}) ->
    State = #state{
        socket = HandshakeInfo#handshake_info.socket,
        server_epoch = HandshakeInfo#handshake_info.server_epoch,
        client_epoch = HandshakeInfo#handshake_info.client_epoch,
        handler_module = HandlerMod
    },
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(ownership, State) -> % Ownership of socket recieved
    inet:setopts(State#state.socket, {active, false}),
    gen_server:cast(self(), handler_init),
    gen_server:cast(self(), connection_init_finish),
    {noreply, State};

handle_cast(handler_init, State) ->
    HandlerMod = State#state.handler_module,
    ConnectionRef = create_connection_ref(),
    HandlerState0 = HandlerMod:init(ConnectionRef),
    case HandlerState0 of
        {ok, HandlerState} ->
            {noreply, State#state{handler_state = HandlerState}};
        {error, Reason} ->
            {stop, {handler, HandlerMod, Reason}, State}
    end;

handle_cast(connection_init_finish, State0) ->
    Timestamp0 = erlang:system_time(millisecond) - State0#state.server_epoch,
    ok = send_window_size(State0#state.socket, Timestamp0, 512),
    Timestamp1 = erlang:system_time(millisecond) - State0#state.server_epoch,
    ok = send_peer_bandwidth(State0#state.socket, Timestamp1, 512, soft),

    inet:setopts(State0#state.socket, {active, once}),
    ok;

handle_cast({set_peer_bandwidth, Size, LimitType}, State0) ->
    Timestamp = erlang:system_time(millisecond) - State0#state.server_epoch,
    ok = send_peer_bandwidth(State0#state.socket, Timestamp, Size, LimitType),
    {noreply, State0#state{recieve_window_acknowledgement_size = Size}};

handle_cast({send, Message}, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State0) ->
    %% Basic Header
    {#basic_header{format = Fmt, csid = CSID} = BasicHeader, Rest0} = unpack_basic_header(Data),

    %%% Chunk Message Header
    if
        is_map_key(CSID, State0#state.chunk_streams) ->
            #chunk_stream{extended_timestamp = Extended} = map_get(CSID, State0#state.chunk_streams);
        true ->
            Extended = false % It better be false
    end,
    {ChunkHeader, NewExtended, Rest1} = unpack_chunk_header(BasicHeader, Extended, Rest0),
    UpdatedStreams = update_chunk_streams(State0#state.chunk_streams, BasicHeader#basic_header.csid, ChunkHeader),

    %%% Chunk Data
    %% Create a new state of chunk stream status
    NewState = if
        %% Check if this is a control message, and react if it is (5.4)
        CSID =:= 2, Fmt =:= 0, (map_get(stream_id, ChunkHeader) =:= 0), (map_get(type, ChunkHeader) =/= 4) ->
            #{type := Type} = ChunkHeader,
            case Type of
                1 -> % Set Chunk Size (5.4.1)
                    <<0:1/integer, SetSize:31/integer>> = Rest1,
                    if SetSize =:= 0 -> throw({chunk_error, zero_size_limit}) end, % "Maximum size SHOULD be at least 128 bytes, and MUST be at least 1 byte"
                    State0#state{recieve_max_chunk_size = SetSize};
                2 -> % Abort Message (5.4.2)
                    <<DropMessage:32/integer>> = Rest1,
                    if
                        is_map_key(DropMessage, UpdatedStreams) ->
                            #{CSID := Stream} = UpdatedStreams,
                            EmptyStream = Stream#chunk_stream{recieved_data = <<>>},
                            ChunkStreams1 = UpdatedStreams#{CSID := EmptyStream},
                            State0#state{chunk_streams = ChunkStreams1};
                        true ->
                            State0
                    end;
                3 -> % Acknowledgement (5.4.3)
                    <<Acknowledgement:32/integer>> = Rest1, % TODO
                    if 
                        Acknowledgement =:= State0#state.sent_sequence ->
                            State0#state{sent_since_last_ack = 0};
                        true ->
                            throw({chunk_error, sequence_mismatch, Acknowledgement, State0#state.sent_sequence})
                    end;
                5 -> % Window Acknowledgement Size (5.4.4)
                    <<WindowSize:32/integer>> = Rest1,
                    State0#state{recieve_window_acknowledgement_size = WindowSize};
                6 -> % Set Peer Bandwidth (5.4.5)
                    <<WindowSize:32/integer, LimitType:8/integer>> = Rest1,
                    LastWindowSize = State0#state.send_window_acknowledgement_size,
                    Timestamp = erlang:system_time(millisecond) - State0#state.server_epoch,
                    case LimitType of
                        0 -> % Hard
                            send_window_size(State0#state.socket, Timestamp, State0#state.recieve_window_acknowledgement_size),
                            State0#state{send_window_acknowledgement_size = WindowSize, send_last_limit = hard};
                        1 when WindowSize < LastWindowSize -> % Soft, smaller
                            send_window_size(State0#state.socket, Timestamp, State0#state.recieve_window_acknowledgement_size),
                            State0#state{send_window_acknowledgement_size = WindowSize, send_last_limit = soft};
                        1 -> % Soft, no effect
                            State0#state{send_last_limit = soft};
                        2 when State0#state.send_last_limit =:= 0 -> % Dynamic, last was Hard
                            send_window_size(State0#state.socket, Timestamp, State0#state.recieve_window_acknowledgement_size),
                            State0#state{send_window_acknowledgement_size = WindowSize, send_last_limit = hard};
                        2 -> % Dynamic, ignore
                            State0
                    end
            end;
        %% If it isn't a control message, check if chunk isn't too large (if it is, throw)
        byte_size(Rest1) > State0#state.recieve_max_chunk_size -> 
            throw({chunk_error, size, State0#state.recieve_max_chunk_size, byte_size(Rest1)});
        %% Is this chunk part of a known chunk stream?
        is_map_key(CSID, UpdatedStreams) ->
            #{CSID := StreamInfo} = UpdatedStreams,
            RecievedSize = byte_size(Rest1),
            CurrentSequence = State0#state.recieved_sequence + RecievedSize, % Update recieved sequence number
            WindowFill = State0#state.recieved_since_last_ack + RecievedSize, % Update recieved bytes since last ack
            CurrentData = StreamInfo#chunk_stream.recieved_data,
            NewData = <<CurrentData/binary, Rest1/binary>>, % TODO: Make this not append to end, if possible
            DataSize = byte_size(NewData),
            CurWindowFill = if 
                                WindowFill =:= State0#state.recieve_window_acknowledgement_size -> % Send an ack if one is due
                                    Timestamp = erlang:system_time(millisecond) - State0#state.server_epoch,
                                    send_acknowledgement(State0#state.socket, Timestamp, CurrentSequence),
                                    0; % Reset window sequence tracking
                                WindowFill > State0#state.recieve_window_acknowledgement_size ->
                                    throw({chunk_error, overfilled_window,
                                        State0#state.recieve_window_acknowledgement_size, WindowFill});
                                true ->
                                    WindowFill
                            end,
            if
                DataSize =:= StreamInfo#chunk_stream.length ->
                    HandlerMod = State0#state.handler_module,
                    NewHandlerState = HandlerMod:recieve(NewData, State0#state.handler_state),
                    NewStreamInfo = StreamInfo#chunk_stream{recieved_data = <<>>, extended_timestamp = NewExtended},
                    NewChunkStreams = UpdatedStreams#{CSID := NewStreamInfo},
                    State0#state{chunk_streams = NewChunkStreams, recieved_sequence = CurrentSequence,
                        recieved_since_last_ack = CurWindowFill, handler_state = NewHandlerState};
                DataSize < StreamInfo#chunk_stream.length ->
                    NewStreamInfo = StreamInfo#chunk_stream{recieved_data = NewData, extended_timestamp = NewExtended},
                    NewChunkStreams = UpdatedStreams#{CSID := NewStreamInfo},
                    State0#state{chunk_streams = NewChunkStreams, recieved_sequence = CurrentSequence,
                        recieved_since_last_ack = CurWindowFill};
                true ->
                    throw({chunk_error, size, StreamInfo#chunk_stream.length, DataSize})
            end;
        %% Something's wrong
        true ->
            throw({chunk_error, csid, CSID, maps:keys(UpdatedStreams)})
    end,
    inet:setopts(Socket, {active, once}),
    {noreply, NewState}.

%% @doc Unpack RTMP chunk basic header.
%% The basic header contains the message format and chunk stream id.
%% Described by section 5.3.1.1 of the spec.
%% @param Data The binary data to unpack.
%% @returns A tuple containing the basic header information (as a record), and the remaining binary
-spec unpack_basic_header(Data :: binary()) -> {basic_header(), binary()}.
unpack_basic_header(Data) ->
    <<Fmt:2/integer, CSID0:6/integer, Rest0/binary>> = Data,
    case CSID0 of
        0 ->
            <<CSID:8/integer, BasicRest/binary>> = Rest0,
            {#basic_header{format = Fmt, csid = CSID + 64}, BasicRest};
        1 ->
            <<CSID:8/integer, CSID1:8/integer, BasicRest/binary>> = Rest0,
            {#basic_header{format = Fmt, csid = CSID1 * 256 + CSID + 64}, BasicRest};
        _Else ->
            {#basic_header{format = Fmt, csid = CSID0}, Rest0}
    end.

%% @doc Unpack RTMP chunk message header.
%% The RTMP chunk message header format is described by the format in the basic header. Depending on the format, it can contain different amounts of data.
%% <ul>
%% <li>Type 0 declares a new message. It contains an absolute timestamp, the message length, the type ID, and the stream id.
%% <ul><li>Additionally, Type 0 MUST be used at the start of a chunk stream or whenever the timestamp goes backwards.</li></ul>
%% </li>
%% <li>Type 1 declares a new message with the same stream ID as the last in the chunk stream. It contains a timestamp delta, the message length, and the type ID of the message.</li>
%% <li>Type 2 only contains a timestamp delta. The chunk has the same stream ID and message length as the last chunk</li>
%% <li>Type 3 has no header. All values are identical to the last chunk.</li>
%% </ul>
%% Aditionally, there may be an extended timestamp included. The extended timestamp is present if the time or time delta of the message is 16777215 (0xFFFFFF), or in the case of a type 3 chunk, if the most recent type 0, 1, or 2 chunk in the same stream had an extended timestamp.
%% The extended timestamp is the next 32 bits of the message.
%% @param BasicHeader The information from the chunk's basic header.
%% @param Extended If the previous chunk indicated an extended timestamp.
%% @param Data The binary data to be unpacked.
%% @returns A tuple containing the chunk message header information, whether or not the next chunk will have an extended timestamp, and the binary information for the rest of the chunk.
-spec unpack_chunk_header(BasicHeader :: basic_header(), Extended :: boolean(), Data :: binary()) -> {chunk_header(), boolean(), binary()}.
unpack_chunk_header(#basic_header{format = Fmt}, Extended, Data) ->
    {HeaderData0, Rest1} = case Fmt of
        0 ->
            <<Timestamp:24/integer, Length:24/integer, TypeID:8/integer, StreamID:32/little-integer, Rest0/binary>> = Data,
            {#{time => Timestamp, length => Length, type => TypeID, stream_id => StreamID}, Rest0};
        1 ->
            <<TimestampDelta:24/integer, Length:24/integer, TypeID:8/integer, Rest0/binary>> = Data,
            {#{time => TimestampDelta, length => Length, type => TypeID}, Rest0};
        2 ->
            <<TimestampDelta:24/integer, Rest0/binary>> = Data,
            {#{time => TimestampDelta}, Rest0};
        3 ->
            {#{}, Data}
    end,
    if 
        Fmt < 3, map_get(time, HeaderData0) =:= 16#FFFFFF ->
            <<Time1:32/integer, Rest2/binary>> = Rest1,
            {HeaderData0#{time := Time1}, true, Rest2};
        Fmt =:= 3, Extended == true ->
            <<Time1:32/integer, Rest2/binary>> = Rest1,
            {HeaderData0#{time := Time1}, true, Rest2};
        true ->
            {HeaderData0, false, Rest1}
    end.

%% @doc Update known chunk streams.
%% Update the map of known chunk streams with new information, be it a new chunk stream or new message info. If the chunk message header doesn't contain a length, it's type 2 or 3, and can be disregarded for this.
%% @param Streams Map of chunk streams. Keys are IDs (integers), and values are chunk stream records.
%% @param CSID The id of the chunk stream to be updated.
%% @param HeaderInfo The basic chunk header containing the info needed for the new chunk stream information.
%% @returns A map of the current chunk streams.
-spec update_chunk_streams(Streams :: #{integer() := #chunk_stream{}}, CSID :: integer(), _HeaderInfo :: chunk_header()) -> #{integer() := #chunk_stream{}}.
update_chunk_streams(Streams, CSID, #{length := Length, type := TypeID, stream_id := StreamID} = _HeaderInfo) ->
    Streams#{CSID => #chunk_stream{length = Length, type = TypeID, stream_id = StreamID, recieved_data = <<>>}};

update_chunk_streams(Streams, CSID, #{length := Length, type := TypeID} = _HeaderInfo) when is_map_key(CSID, Streams) ->
    #{CSID := CurrentStream} = Streams,
    NewStream = CurrentStream#chunk_stream{length = Length, type = TypeID, recieved_data = <<>>},
    Streams#{CSID := NewStream};

update_chunk_streams(Streams, _CSID, Map) when is_map_key(length, Map) == false ->
    Streams.

%% @doc Create the opaque record used to locate this worker for the purposes of sending messages
%% Called before sending it to the handler.
%% @returns The opaque record used to refer to the calling worker.
-spec create_connection_ref() -> connection_worker_ref().
create_connection_ref() ->
    {self()}.

%% @doc Get the PID from a connection_worker_ref() record.
%% The record is opaque, and ideally any users of the API shouldn't even try to get the Pid, but we need a way to get it, ideally in just one place.
%% @param Ref The connection worker reference.
%% @returns The pid for the connection worker.
-spec get_worker_pid(Ref :: connection_worker_ref()) -> pid().
get_worker_pid({Pid}) ->
    Pid.

%% @doc Set the bandwidth limit.
%% @param Socket Socket to send the message on.
%% @param Time The current time offset from the Epoch.
%% @param Size The limit to set.
%% @param LimitType The type of limit to set
%% @returns The return types from gen_tcp:send/2.
-spec send_peer_bandwidth(Socket :: gen_tcp:socket(), Time :: non_neg_integer(), Size :: 1..16#FFFFFFFF, LimitType :: soft | hard | dynamic) -> ok | {error, closed | {timeout, binary()}, inet:posix()}.
send_peer_bandwidth(Socket, Time, Size, LimitType) ->
    BasicHeader = <<0:2/integer, 2:6/integer>>,
    MessageHeader0 = <<5:24/integer, 6:8/integer, 0:32/integer>>,
    MessageHeader = if
        Time >= 16#FFFFFF -> <<16#FFFFFF:24/integer, MessageHeader0/binary, Time:32/integer>>;
        true -> <<Time:24/integer, MessageHeader0/binary>>
        end,
    TypeBinary = case LimitType of
        hard -> <<0:8/integer>>;
        soft -> <<1:8/integer>>;
        dynamic -> <<2:8/integer>>
    end,
    Message = <<BasicHeader/binary, MessageHeader/binary, Size:32/integer, TypeBinary/binary>>,
    gen_tcp:send(Socket, Message).

%% @doc Set the window size.
%% @param Socket The socket to send the message with.
%% @param Time The time since the server epoch.
%% @param Size The size to set the window to.
%% @returns The return types from gen_tcp:send/2.
-spec send_window_size(Socket :: gen_tcp:socket(), Time :: non_neg_integer(), Size :: 1..16#FFFFFFFF) -> ok | {error, closed | {timeout, binary()}, inet:posix()}.
send_window_size(Socket, Time, Size) ->
    BasicHeader = <<0:2/integer, 2:6/integer>>,
    MessageHeader0 = <<4:24/integer, 5:8/integer, 0:32/integer>>,
    MessageHeader = if
        Time >= 16#FFFFFF -> <<16#FFFFFF:24/integer, MessageHeader0/binary, Time:32/integer>>;
        true -> <<Time:24/integer, MessageHeader0/binary>>
        end,
    Message = <<BasicHeader/binary, MessageHeader/binary, Size:32/integer>>,
    gen_tcp:send(Socket, Message).

%% @doc Send an acknowledgement message.
%% @param Socket The socket to send the message with.
%% @param Time The time since the server epoch.
%% @param SequenceNumber The sequence number to send the acknowledgement for.
%% @returns The return types from gen_tcp:send/2.
-spec send_acknowledgement(Socket :: gen_tcp:socket(), Time :: non_neg_integer(), SequenceNumber :: 1..16#FFFFFFFF) -> ok | {error, closed | {timeout, binary()}, inet:posix()}.
send_acknowledgement(Socket, Time, SequenceNumber) ->
    BasicHeader = <<0:2/integer, 2:6/integer>>,
    MessageHeader0 = <<4:24/integer, 3:8/integer, 0:32/integer>>,
    MessageHeader = if
        Time >= 16#FFFFFF -> <<16#FFFFFF:24/integer, MessageHeader0/binary, Time:32/integer>>;
        true -> <<Time:24/integer, MessageHeader0/binary>>
        end,
    Message = <<BasicHeader/binary, MessageHeader/binary, SequenceNumber:32/integer>>,
    gen_tcp:send(Socket, Message).

%% todo: handle termation reasons
terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
