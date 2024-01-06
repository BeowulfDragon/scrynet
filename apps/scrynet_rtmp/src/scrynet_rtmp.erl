-module(scrynet_rtmp).

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

decode_amf(Data) ->
    ok.

decode_amf0(Data) ->
    ok.

decode_amf0_key(Data) ->
    <<KeyLength:16/integer, Key:KeyLength/binary, Rest/binary>> = Data,
    {Key, Rest}.

%% Number (0x00), IEEE 64-bit double
decode_amf0_value(<<16#00:8/integer, Value:64/float, Rest/binary>> = _Data, Index) ->
    {Value, Index, Rest};

%% Boolean (0x01), single byte value of 0x00 or 0x01
decode_amf0_value(<<16#01:8/integer, Value:8/integer, Rest/binary>> = _Data, Index) ->
    Boolean = case Value of
                0 ->
                    false;
                1 ->
                    true
            end,
    {Boolean, Index, Rest};

%% String (0x02), 16-bit integer followed by a utf-8 string of that length
% Storing the string as a binary because binaries and also the utf-8 type for bit syntax doesn't allow for size
decode_amf0_value(<<16#02:8/integer, Length:16/integer, Value:Length/binary, Rest/binary>> = _Data, Index) ->
    {Value, Index, Rest};

%% Object (0x03), set of key-value pairs. Complex object.
% TODO
decode_amf0_value(<<16#03:8/integer, Rest0/binary>> = _Data, Index0) ->
    {Object, Index1, Rest} = decode_amf0_object(Rest0, Index0),
    NewIndex = Index1 ++ [Object],
    {Object, NewIndex, Rest};

%% Movieclip (0x04), reserved, not supported

%% Null (0x05). Is null.
decode_amf0_value(<<16#05:8/integer, Rest/binary>> = _Data, Index) ->
    {null, Index, Rest};

%% Undefined (0x06). Is not null, but is null-like. God I love ECMAScript.
decode_amf0_value(<<16#06:8/integer, Rest/binary>> = _Data, Index) ->
    {undefined, Index, Rest};

%% Reference (0x07). If a complex object (anonymous, typed, array or ecma-array) appears more than once, it is sent as a reference
decode_amf0_value(<<16#07:8/integer, ReferenceMarker:16/integer, Rest/binary>> = _Data, Index) ->
    {lists:nth(ReferenceMarker + 1, Index), Index, Rest};


%% ECMA Array (0x08), an associative array. Contains non-ordinal indices. Complex object.
decode_amf0_value(<<16#08:8/integer, Count:32/integer, Rest0/binary>> = _Data, Index0) ->
    {Array, Index1, Rest} = decode_amf0_ecma_array(Count, Rest0, Index0),
    NewIndex = Index1 ++ [Array],
    {Array, NewIndex, Rest};

%% Object End (0x09). Shouldn't come up outside of object stuff.
% decode_amf0_value(<<16#0000:16/integer, 16#09:8/integer, Rest/binary>> = _Data, Index) -> {fucked_up, Index, Rest};

%% Strict Array (0x0A).
decode_amf0_value(<<16#0A:8/integer, Count:32/integer, Rest0/binary>> = _Data, Index0) ->
    {Array, Index1, Rest1} = decode_amf0_strict_array(Count, Rest0, Index0),
    NewIndex = Index1 ++ [Array],
    {Array, NewIndex, Rest1};

%% Date (0x0B). Last 2 bytes are reserved for timezones, but is not supported
decode_amf0_value(<<16#0B:8/integer, Date:64/float, 16#0000:16/integer, Rest/binary>>, Index) ->
    {Date, Index, Rest};

%% Long String (0x0C). For when you have a long long striiiiiiinggggg (length in bytes larger than expressable in 16 bits).
decode_amf0_value(<<16#0C:8/integer, Length:32/integer, String:Length/binary, Rest/binary>>, Index) ->
    {String, Index, Rest};

%% Unsupported Type (0x0D). Type can't be serialized. Can probably be safely ignored in a post-flash world
% decode_amf0_value(<<16#0D:8/integer, Rest/binary>>, Index) -> {fucked_up, Index, Rest};

%% RecordSet (0x0E). Not supported, reserved for future use

%% XML document (0x0F). Is a long string, but specified as an xml document. xmerl is in the regular erlang distribution, but I'm going to leave it unparsed for now.
decode_amf0_value(<<16#0F:8/integer, Length:32/integer, String:Length/binary, Rest/binary>>, Index) ->
    {String, Index, Rest};

%% Typed Object (0x10). An object with an alias registered for the class. Complex object.
decode_amf0_value(<<16#10:8/integer, NameLength:16/integer, Name:NameLength/binary, Rest0/binary>>, Index0) ->
    {ObjectBody, Index1, Rest1} = decode_amf0_object(Rest0, Index0),
    Object = {Name, ObjectBody},
    NewIndex = Index1 ++ [Object],
    {Object, NewIndex, Rest1}.

decode_amf0_object(Data, Index) ->
    decode_amf0_object(Data, Index, #{}).

decode_amf0_object(<<0:16/integer, 16#09:8/integer, Rest/binary>> = _Data, Index, Acc) ->
    {Acc, Index, Rest};

decode_amf0_object(Data, Index, Acc0) ->
    {Key, Rest0} = decode_amf0_key(Data),
    {Value, NewIndex, Rest1} = decode_amf0_value(Rest0, Index),
    Acc = Acc0#{Key => Value},
    decode_amf0_object(Rest1, NewIndex, Acc).

decode_amf0_ecma_array(Count, Data, Index) ->
    decode_amf0_ecma_array(Count, Data, #{}, Index).

decode_amf0_ecma_array(0, <<16#0000:16/integer, 16#09:8/integer, Rest/binary>> = _Data, Acc, Index) ->
    {Acc, Index, Rest};

decode_amf0_ecma_array(Count, Data, Acc0, Index) ->
    {Key, Rest0} = decode_amf0_key(Data),
    {Value, NewIndex, Rest1} = decode_amf0_value(Rest0, Index),
    Acc = Acc0#{Key => Value},
    decode_amf0_ecma_array(Count - 1, Rest1, Acc, NewIndex).
    
decode_amf0_strict_array(Count, Data, Index) ->
    decode_amf0_strict_array(Count, Data, [], Index).

decode_amf0_strict_array(0, Data, Acc, Index) ->
    {lists:reverse(Acc), Index, Data};

decode_amf0_strict_array(Count, Data, Acc, Index) ->
    {Value, NewIndex, Rest} = decode_amf0_value(Data, Index),
    decode_amf0_strict_array(Count - 1, Rest, [Value | Acc], NewIndex).


