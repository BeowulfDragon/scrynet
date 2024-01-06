-module(scrynet_rtmp_handler).
-behaviour(gen_server).

-include("handshake_info.hrl").
%% API
-export([start_and_transfer/1, start_link/0, give_socket/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(chunk_stream, {
            length :: integer(),
            type :: integer(),
            stream_id :: integer(),
            recieved_data :: binary(),
            time :: integer(),
            extended_timestamp :: boolean()
    }).

-record(state, {socket :: gen_tcp:socket(),
                ownership :: boolean(),
                chunk_streams = #{} :: #{integer() := #chunk_stream{}},
                finished_message :: fun(),
                server_epoch :: integer(),
                client_epoch :: integer(),
                send_max_chunk_size = 128,
                recieve_max_chunk_size = 128,
                recieve_window_acknowledgement_size :: integer(),
                recieved_sequence = 0,
                recieved_since_last_ack = 0,
                recieve_last_limit :: soft | hard, % Last limit type set for the peer by here
                send_window_acknowledgement_size :: integer(),
                sent_sequence = 0,
                sent_since_last_ack = 0,
                send_last_limit :: soft | hard}). % Last limit type set by the peer for here

-record(basic_header, {format :: 0 | 1 | 2 | 3, csid :: 0..65599}).
-type basic_header() :: #basic_header{}.

-type chunk_header() :: #{time := integer(), length => 0..16777215, type => 0..255, stream_id => 0..4294967295}.

-spec give_socket(Socket :: gen_tcp:socket(), Pid :: pid()) -> ok.
give_socket(Socket, Pid) ->
    gen_tcp:controlling_process(Socket, Pid),
    gen_server:cast().

-spec start_and_transfer(HandshakeInfo :: #handshake_info{}) -> supervisor:startchild_ret().
start_and_transfer(HandshakeInfo) ->
    Ret = scrynet_rtmp_handler_sup:start_child(HandshakeInfo),
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

-spec start_handler(HandshakeInfo :: #handshake_info{}) -> supervisor:startchild_ret().
start_handler(HandshakeInfo) ->
    scrynet_rtmp_handler_sup:start_child(HandshakeInfo).

start_link(HandshakeInfo, OnConnect) ->
    gen_server:start_link(?MODULE, {HandshakeInfo, OnConnect}, []).

init({HandshakeInfo, OnConnect}) ->
    OnFinished = OnConnect(HandshakeInfo),
    State = #state{
        socket = HandshakeInfo#handshake_info.socket,
        ownership = false,
        finished_message = OnFinished,
        server_epoch = HandshakeInfo#handshake_info.server_epoch,
        client_epoch = HandshakeInfo#handshake_info.client_epoch,
        recieve_window_acknowledgement_size
    },
    {ok, State}.

send_message(Message, HandlerPID) -> 
    gen_server:cast(HandlerPID, {send, Message}).

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(ownership, State) ->
    inet:setopts(State#state.socket, {active, once}),
    {noreply, State};

handle_cast({send, Message}, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State0) ->
    %% Basic Header
    {#basic_header{format = Fmt, csid = CSID} = BasicHeader, Rest0} = unpack_basic_header(Data),
    %%% Chunk Message Header
    %% Pattern match header type and update map of chunk streams if needed
    %{NewStreams, HeaderInfo, Rest1} = case BasicHeader#basic_header.format of
    %    0 ->
    %        <<Timestamp:24/integer, Length:24/integer, TypeID:8/integer, StreamID:32/little-integer, ChunkRest/binary>> = Rest1,
    %        HeaderData = #{delta => Timestamp, length => Length, type => TypeID, stream_id => StreamID},
    %        UpdatedStreams = update_chunk_streams(State0#state.chunk_streams, CSID, HeaderData),
    %        {UpdatedStreams, HeaderData, ChunkRest};
    %    1 ->
    %        <<TimestampDelta:24/integer, Length:24/integer, TypeID:8/integer, ChunkRest/binary>> = Rest1,
    %        HeaderData = #{delta => TimestampDelta, length => Length, type => TypeID},
    %        UpdatedStreams = update_chunk_streams(State0#state.chunk_streams, CSID, HeaderData),
    %        {UpdatedStreams, HeaderData, ChunkRest};
    %    2 ->
    %        <<TimestampDelta:24/integer, ChunkRest/binary>> = Rest1,
    %        HeaderData = #{delta => TimestampDelta},
    %        {State0#state.chunk_streams, HeaderData, ChunkRest};
    %    3 ->
    %        {State0#state.chunk_streams, #{}, Rest1} % TODO: Extended timestamps (5.3.1.3)
    %end,
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
                    case LimitType of
                        0 -> % Hard
                            State0#state{send_window_acknowledgement_size = WindowSize, send_last_limit = hard};
                        1 when WindowSize < State0#state.recieve_window_acknowledgement_size -> % Soft, smaller
                            State0#state{send_window_acknowledgement_size = WindowSize, send_last_limit = soft};
                        1 -> % Soft, no effect
                            State0#state{send_last_limit = soft};
                        2 when State0#state.send_last_limit =:= 0 -> % Dynamic, last was Hard
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
                                    AckMsg = create_acknowledgement_message(CurrentSequence, State0#state.server_epoch),
                                    gen_tcp:send(Socket, AckMsg),
                                    0; % Reset window sequence tracking
                                WindowFill > State0#state.recieve_window_acknowledgement_size ->
                                    throw({chunk_error, overfilled_window, State0#state.recieve_window_acknowledgement_size, WindowFill});
                                true ->
                                    WindowFill
                            end,
            if
                DataSize =:= StreamInfo#chunk_stream.length ->
                    Finished = State0#state.finished_message,
                    Finished(NewData),
                    NewStreamInfo = StreamInfo#chunk_stream{recieved_data = <<>>, extended_timestamp = NewExtended},
                    NewChunkStreams = UpdatedStreams#{CSID := NewStreamInfo},
                    State0#state{chunk_streams = NewChunkStreams, recieved_sequence = CurrentSequence, recieved_since_last_ack = CurWindowFill};
                DataSize < StreamInfo#chunk_stream.length ->
                    NewStreamInfo = StreamInfo#chunk_stream{recieved_data = NewData, extended_timestamp = NewExtended},
                    NewChunkStreams = UpdatedStreams#{CSID := NewStreamInfo},
                    State0#state{chunk_streams = NewChunkStreams, recieved_sequence = CurrentSequence, recieved_since_last_ack = CurWindowFill};
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

%% @doc Create a protocl control message type 3 acknowledgement.
%% Pack a sequence number and current server time into a window acknowledgement message.
%% @param SequenceNumber The current sequence number for recieved messages.
%% @param Epoch The server-side epoch for this connection (we are the server).
%% @returns A binary to be sent as an acknowledgement message.
-spec create_acknowledgement_message(SequenceNumber :: integer(), Epoch :: integer()) -> binary().
create_acknowledgement_message(SequenceNumber, Epoch) ->
    Time = erlang:system_time(millisecond) - Epoch,
    % Format, CSID, timestamp, length, type ID, stream ID, (extended timestamp), sequence number
    if 
        Time >= 16#FFFFFF -> 
            <<0:2/integer, 2:6/integer, 16#FFFFFF:24/integer, 32:32/integer, 3:8/integer, 0:32/integer, Time:32/integer, SequenceNumber:32/integer >>;
        true -> 
            <<0:2/integer, 2:6/integer, Time:24/integer, 32:32/integer, 3:8/integer, 0:32/integer, SequenceNumber:32/integer>>
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
