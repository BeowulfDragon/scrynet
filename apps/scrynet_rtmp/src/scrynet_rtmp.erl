-module(scrynet_rtmp).

-export([start_listener/2]).

-record(rtmp_message, {
    type :: integer(),
    length :: integer(),
    timestamp :: integer(),
    stream_id :: integer(),
    payload :: binary()
    }).

-type rtmp_message() :: #rtmp_message{}.


-export_type([rtmp_message/0]).

unpack_rtmp_message(Data) ->
    <<Type:8/integer, Length:24/integer, Timestamp:32/integer, StreamID:24/integer, Rest/binary>> = Data,
    #rtmp_message{type = Type, length = Length, timestamp = Timestamp, stream_id = StreamID, payload = Rest}.

-spec start_listener(Port :: non_neg_integer(), HandlerMod :: atom()) -> supervisor:startchild_ret().
start_listener(Port, HandlerMod) ->
    scrynet_rtmp_acceptor_pool_sup:start_acceptor_pool(Port, HandlerMod).