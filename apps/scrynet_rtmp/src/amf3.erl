
-module(amf3).

-record(amf3_index, {
    strings :: list(),
    complex_objects :: list(),
    object_traits :: list()
    }).

-record(amf3_array, {
    associative = #{} :: map(),
    dense = [] :: list()
    }).

-type amf3_index() :: #amf3_index{}.

-spec new_amf3_index() -> amf3_index().
new_amf3_index() ->
    #amf3_index{strings = [], complex_objects = [], object_traits = []}.

-spec get_amf3_reference(IndexType :: string | complex | trait, Position :: non_neg_integer(), Index :: amf3_index()) -> term().
get_amf3_reference(string, Pos, Index) ->
    lists:nth(Pos + 1, Index#amf3_index.strings);

get_amf3_reference(complex, Pos, Index) ->
    lists:nth(Pos + 1, Index#amf3_index.complex_objects);

get_amf3_reference(trait, Pos, Index) ->
    lists:nth(Pos + 1, Index#amf3_index.object_traits).

-spec add_amf3_reference(IndexType :: string | complex | trait, Value :: term(), Index :: amf3_index()) -> amf3_index().
add_amf3_reference(string, Value, Index) ->
    Current = Index#amf3_index.strings,
    Index#amf3_index{strings = Current ++ [Value]};

add_amf3_reference(complex, Value, Index) ->
    Current = Index#amf3_index.complex_objects,
    Index#amf3_index{complex_objects = Current ++ [Value]};

add_amf3_reference(trait, Value, Index) ->
    Current = Index#amf3_index.object_traits,
    Index#amf3_index{object_traits = Current ++ [Value]}.

decode_amf3() ->
    ok.

decode_amf3_u29(<<0:1/integer, Number:7/integer, Rest/binary>>) ->
    {Number, Rest};

decode_amf3_u29(<<1:1/integer, Number1:7/integer, 0:1/integer, Number2:7/integer, Rest/binary>>) ->
    Value = (Number1 bsl 7) + Number2,
    {Value, Rest};

decode_amf3_u29(<<1:1/integer, Number1:7/integer, 1:1/integer, Number2:7/integer, 0:1/integer, Number3:7/integer, Rest/binary>>) ->
    Value = (Number1 bsl 14) + (Number2 bsl 7) + Number3,
    {Value, Rest};

decode_amf3_u29(<<1:1/integer, Number1:7/integer, 1:1/integer, Number2:7/integer, 1:1/integer, Number3:15/integer, Rest/binary>>) ->
    Value = (Number1 bsl 22) + (Number2 bsl 15) + Number3,
    {Value, Rest}.

%% For the integer type, the U29 is actually *signed*. So it's not actually U29 is it?
decode_amf3_i29(<<0:1/integer, Number:7/signed-integer, Rest/binary>>) ->
    {Number, Rest};

decode_amf3_i29(<<1:1/integer, Number1:7/bits, 0:1/integer, Number2:7/bits, Rest/binary>>) ->
    <<Value:14/signed-integer>> = <<Number1/bits, Number2/bits>>,
    {Value, Rest};

decode_amf3_i29(<<1:1/integer, Number1:7/integer, 1:1/integer, Number2:7/integer, 0:1/integer, Number3:7/integer, Rest/binary>>) ->
    <<Value:21/signed-integer>> = <<Number1/bits, Number2/bits, Number3/bits>>,
    {Value, Rest};

decode_amf3_i29(<<1:1/integer, Number1:7/integer, 1:1/integer, Number2:7/integer, 1:1/integer, Number3:15/integer, Rest/binary>>) ->
    <<Value:29/signed-integer>> = <<Number1/bits, Number2/bits, Number3/bits>>,
    {Value, Rest}.

%% Undefined (0x00)
decode_amf3_value(<<16#00:8/integer, Rest/binary>>, Index) ->
    {undefined, Index, Rest};

%% Null (0x01)
decode_amf3_value(<<16#01:8/integer, Rest/binary>>, Index) ->
    {null, Index, Rest};

%% False (0x02)
decode_amf3_value(<<16#02:8/integer, Rest/binary>>, Index) ->
    {false, Index, Rest};

%% True (0x03)
decode_amf3_value(<<16#03:8/integer, Rest/binary>>, Index) ->
    {true, Index, Rest};

%% Integer (0x04)
decode_amf3_value(<<16#04:8/integer, Rest0/binary>>, Index) ->
    {Value, Rest} = decode_amf3_i29(Rest0),
    {Value, Index, Rest};

%% Double (0x05)
decode_amf3_value(<<16#05:8/integer, Value:32/float, Rest/binary>>, Index) ->
    {Value, Index, Rest};

%% String (0x06)
decode_amf3_value(<<16#06:8/integer, Rest0/binary>>, Index0) ->
    {U29VR, Rest1} = decode_amf3_u29(Rest0),
    LengthRef = U29VR bsr 1,
    if
        (U29VR rem 2) =:= 0 -> % reference
            Value = get_amf3_reference(string, LengthRef, Index0),
            {Value, Index0, Rest1};
        true -> % value
            <<Value:LengthRef/binary, Rest2/binary>> = Rest1,
            Index1 = add_amf3_reference(string, Value, Index0),
            {Value, Index1, Rest2}
    end;

%% XMLDocument (0x07). Pretty much just string again, but complex object
decode_amf3_value(<<16#07:8/integer, Rest0/binary>>, Index0) ->
    {U29VR, Rest1} = decode_amf3_u29(Rest0),
    LengthRef = U29VR bsr 1,
    if
        (U29VR rem 2) =:= 0 -> % reference
            Value = get_amf3_reference(complex, LengthRef, Index0),
            {Value, Index0, Rest1};
        true -> % value
            <<Value:LengthRef/binary, Rest2/binary>> = Rest1,
            Index1 = add_amf3_reference(complex, Value, Index0),
            {Value, Index1, Rest2}
    end;

%% Date (0x08). Complex object. For some reason.
decode_amf3_value(<<16#08:8/integer, Rest0/binary>>, Index0) ->
    {U29VR, Rest1} = decode_amf3_u29(Rest0),
    LengthRef = U29VR bsr 1,
    if
        (U29VR rem 2) =:= 0 ->
            Value = get_amf3_reference(complex, LengthRef, Index0),
            {Value, Index0, Rest1};
        true ->
            <<Value:64/float, Rest2/binary>> = Rest0,
            Index1 = add_amf3_reference(complex, Value, Index0),
            {Value, Index1, Rest2}
    end;

%% Array (0x09).
decode_amf3_value(<<16#09:8/integer, Rest0/binary>>, Index0) ->
    {U29VR, Rest1} = decode_amf3_u29(Rest0),
    LengthRef = U29VR bsr 1,
    if
        (U29VR rem 2) =:= 0 ->
            Value = get_amf3_reference(complex, LengthRef, Index0),
            {Value, Index0, Rest1};
        true ->
            {Value, Index1, Rest2} = decode_amf3_array(LengthRef, Rest1, Index0),
            Index2 = add_amf3_reference(complex, Value, Index0),
            {Value, Index2, Rest2}
    end;

%% Object (0x0A).
% Traits are the names of fields of an object.
decode_amf3_value(<<16#0A:8/integer, Rest0/binary>>, Index0) ->
    {U29VR, Rest1} = decode_amf3_u29(Rest0),
    LengthRef = U29VR bsr 1,
    if
        (U29VR rem 2) =:= 0 -> % U29O-ref
            Value = get_amf3_reference(complex, LengthRef, Index0),
            {Value, Index0, Rest1};
        (U29VR rem 4) =:= 2#01 -> % U29O-traits-ref, so list of traits is in the index
            RealLengthRef = LengthRef bsr 1,
            Traits = get_amf3_reference(trait, RealLengthRef, Index0),
            decode_amf3_object_values(Traits, Rest1, Index0); % This returns {Value, Rest, Index}
        (U29VR rem 8) =:= 2#111 -> % U29O-traits-ext. This is for something with an "indeterminable number of bytes", so TODO
            % If I had to do this, it'd probably be with a hashmap with keys being the class-name and values being functions which decode what's needed.
            throw({amf3_decoding_error, u29o-traits-ext});
        true -> % U29O-traits, not dynamic, which means class-name, list of trait names (or string references), followed by trait values
            {ClassNameLength, Rest2} = decode_amf3_u29(Rest1),
            {ClassName, Index1, Rest3} = if
                (ClassNameLength rem 2) =:= 0 ->
                    Value = get_amf3_reference(string, ClassNameLength, Index0),
                    {Value, Index0, Rest2};
                true ->
                    <<Value:ClassNameLength/binary, IfRest/binary>> = Rest2,
                    IfIndex = add_amf3_reference(string, Value, Index0),
                    {Value, IfIndex, IfRest}
            end,
            TraitCount = U29VR bsr 4,
            {Traits, Rest4, Index2} = decode_amf3_object_traits(TraitCount, Rest3, Index1),
            Index3 = add_amf3_reference(trait, Traits, Index2),
            {Object, Rest5, Index4} = decode_amf3_object_values(Traits, Rest4, Index3),
            {FullObject, Rest6, Index5} = if
                (U29VR bsr 3) rem 2 =:= 2#1 -> % Dynamic. We know the first 3 bits to be 011 by process of elimination, so check the fourth bit
                    {Dynamics, IfRest2, IfIndex2} = decode_amf3_object_dynamics(Rest5, Index4),
                    Obj = #{class_name => ClassName, object => Object, dynamic => Dynamics},
                    {Obj, IfRest2, IfIndex2};
                true ->
                    Obj = #{class_name => ClassName, object => Object},
                    {Obj, Rest5, Index4}
            end,
            Index6 = add_amf3_reference(complex, FullObject, Index5),
            {FullObject, Rest6, Index6}
    end;

%% XML (0x0B). 
decode_amf3_value(<<16#0B:8/integer, Rest0/binary>>, Index0) ->
    {U29VR, Rest1} = decode_amf3_u29(Rest0),
    LengthRef = U29VR bsr 1,
    if
        (U29VR rem 2) =:= 0 -> % U29O-ref
            Value = get_amf3_reference(complex, LengthRef, Index0),
            {Value, Index0, Rest1};
        true ->
            <<Value:LengthRef/binary, Rest2/binary>> = Rest1,
            Index1 = add_amf3_reference(complex, Value, Index0),
            {Value, Index1, Rest2}
    end;

%% ByteArray (0x0C)
decode_amf3_value(<<16#0C:8/integer, Rest0/binary>>, Index0) ->
    {U29VR, Rest1} = decode_amf3_u29(Rest0),
    LengthRef = U29VR bsr 1,
    if
        (U29VR rem 2) =:= 0 ->
            Value = get_amf3_reference(complex, LengthRef, Index0),
            {Value, Index0, Rest1};
        true ->
            <<Value:LengthRef/binary, Rest2/binary>> = Rest1,
            Index1 = add_amf3_reference(complex, Value, Index0),
            {Value, Index1, Rest2}
    end;

%% Vector (Int) (0x0D)
decode_amf3_value(<<16#0D:8/integer, Rest0/binary>>, Index0) ->
    {U29VR, Rest1} = decode_amf3_u29(Rest0),
    LengthRef = U29VR bsr 1,
    <<_FixedVector:8/integer, Rest2/binary>> = Rest1, % Not sure how much this matters for non-actionscript
    if
        (U29VR rem 2) =:= 0 ->
            Value = get_amf3_reference(complex, LengthRef, Index0),
            {Value, Index0, Rest1};
        true ->
            Read = fun 
                Go(0, Data, Acc) ->
                    {lists:reverse(Acc), Data};
                Go(Size, Data, Acc0) ->
                    <<X:32/signed-integer, Rest/binary>> = Data,
                    Acc = [X] ++ Acc0,
                    Go(Size - 1, Rest, Acc)
                end,
            {Value, Rest3} = Read(LengthRef, Rest2, []),
            Index1 = add_amf3_reference(complex, Value, Index0),
            {Value, Index1, Rest3}
    end;

%% Vector (UInt) (0x0E)
decode_amf3_value(<<16#0E:8/integer, Rest0/binary>>, Index0) ->
    {U29VR, Rest1} = decode_amf3_u29(Rest0),
    LengthRef = U29VR bsr 1,
    <<_FixedVector:8/integer, Rest2/binary>> = Rest1, % Not sure how much this matters for non-actionscript
    if
        (U29VR rem 2) =:= 0 ->
            Value = get_amf3_reference(complex, LengthRef, Index0),
            {Value, Index0, Rest1};
        true ->
            Read = fun 
                Go(0, Data, Acc) ->
                    {lists:reverse(Acc), Data};
                Go(Size, Data, Acc0) ->
                    <<X:32/integer, Rest/binary>> = Data,
                    Acc = [X] ++ Acc0,
                    Go(Size - 1, Rest, Acc)
                end,
            {Value, Rest3} = Read(LengthRef, Rest2, []),
            Index1 = add_amf3_reference(complex, Value, Index0),
            {Value, Index1, Rest3}
    end;

%% Vector (Double) (0x0F)
decode_amf3_value(<<16#0F:8/integer, Rest0/binary>>, Index0) ->
    {U29VR, Rest1} = decode_amf3_u29(Rest0),
    LengthRef = U29VR bsr 1,
    <<_FixedVector:8/integer, Rest2/binary>> = Rest1, % Not sure how much this matters for non-actionscript
    if
        (U29VR rem 2) =:= 0 ->
            Value = get_amf3_reference(complex, LengthRef, Index0),
            {Value, Index0, Rest1};
        true ->
            Read = fun 
                Go(0, Data, Acc) ->
                    {lists:reverse(Acc), Data};
                Go(Size, Data, Acc0) ->
                    <<X:64/float, Rest/binary>> = Data,
                    Acc = [X] ++ Acc0,
                    Go(Size - 1, Rest, Acc)
                end,
            {Value, Rest3} = Read(LengthRef, Rest2, []),
            Index1 = add_amf3_reference(complex, Value, Index0),
            {Value, Index1, Rest3}
    end;

%% Vector (Object) (0x10)
decode_amf3_value(<<16#10:8/integer, Rest0/binary>>, Index0) ->
    {U29VR, Rest1} = decode_amf3_u29(Rest0),
    LengthRef = U29VR bsr 1,
    <<_FixedVector:8/integer, Rest2/binary>> = Rest1, % Not sure how much this matters for non-actionscript
    if
        (U29VR rem 2) =:= 0 ->
            Value = get_amf3_reference(complex, LengthRef, Index0),
            {Value, Index0, Rest1};
        true ->
            {ObjectTypeNameVR, Rest3} = decode_amf3_u29(Rest2),
            ObjectTypeNameLengthRef = ObjectTypeNameVR bsr 1,
            {ObjectTypeName, Index1, Rest4} = if
                (ObjectTypeNameVR rem 2) =:= 0 ->
                    Name = get_amf3_reference(string, ObjectTypeNameLengthRef, Index0),
                    {Name, Index0, Rest3};
                true ->
                    <<Name:ObjectTypeNameLengthRef/binary, IfRest/binary>> = Rest3,
                    IfIndex = add_amf3_reference(string, Name, Index0),
                    {Name, IfIndex, IfRest}
            end,
            Read = fun 
                Go(0, Data, GoIndex, Acc) ->
                    {lists:reverse(Acc), GoIndex, Data};
                Go(Size, Data, GoIndex0, Acc0) ->
                    {Val, GoIndex1, GoRest} = decode_amf3_value(Data, GoIndex0),
                    Acc = [Val] ++ Acc0,
                    Go(Size - 1, GoRest, GoIndex1, Acc)
                end,
            {Value, Index1, Rest5} = Read(LengthRef, Rest4, []),
            Index2 = add_amf3_reference(complex, Value, Index1), % Just now realizing that there may be a need for reserving reference spots, because this is probably now how AMF expects things to work
            {Value, Index2, Rest5}
    end;

%% Dictionary (0x11)
decode_amf3_value(<<16#11:8/integer, Rest0/binary>>, Index0) ->
    {U29VR, Rest1} = decode_amf3_u29(Rest0),
    LengthRef = U29VR bsr 1,
    <<_WeakKeys:8/integer, Rest2/binary>> = Rest1, % I do not have enough power over the ERTS GC to do this
    if
        (U29VR rem 2) =:= 0 ->
            Value = get_amf3_reference(complex, LengthRef, Index0),
            {Value, Index0, Rest1};
        true ->
            Read = fun 
                Go(0, Data, GoIndex, Acc) ->
                    {Acc, GoIndex, Data};
                Go(Size, Data, GoIndex0, Acc0) ->
                    {Key, GoIndex1, GoRest0} = decode_amf3_value(Data, GoIndex0),
                    {Val, GoIndex2, GoRest1} = decode_amf3_value(GoRest0, GoIndex1),
                    Acc = Acc0#{Key => Val},
                    Go(Size - 1, GoRest1, GoIndex2, Acc)
            end,
            {Value, Index1, Rest3} = Read(LengthRef, Rest2, Index0, #{}),
            Index2 = add_amf3_reference(complex, Value, Index1),
            {Value, Index2, Rest3}
    end.

decode_amf3_array(Size, Data, Index) when Size > 0 ->
    decode_amf3_array(assoc, Size, Data, Index, #amf3_array{}).

decode_amf3_array(assoc, Size, <<16#0101:16/integer, Rest/binary>>, Index, Acc0) ->
    decode_amf3_array(strict, Size, Rest, Index, Acc0);

decode_amf3_array(assoc, Size, Data, Index0, Acc0) ->
    {U29VR, Rest0} = decode_amf3_u29(Data),
    LengthRef = U29VR bsr 1,
    OldAssoc = Acc0#amf3_array.associative,
    if
        (U29VR rem 2) =:= 0 ->
            Key = get_amf3_reference(string, LengthRef, Index0),
            {Value, Index1, Rest1} = decode_amf3_value(Rest0, Index0),
            decode_amf3_array(assoc, Size, Rest1, Index1, Acc0#amf3_array{associative = OldAssoc#{Key => Value}});
        true ->
            <<Key:LengthRef/binary, Rest1/binary>> = Rest0,
            Index1 = add_amf3_reference(string, Key, Index0),
            {Value, Index2, Rest2} = decode_amf3_value(Rest1, Index1),
            decode_amf3_array(assoc, Size, Rest2, Index2, Acc0#amf3_array{associative = OldAssoc#{Key => Value}})
    end;

decode_amf3_array(dense, 0, Data, Index, Acc0) ->
    UnreversedArray = Acc0#amf3_array.dense,
    {Acc0#amf3_array{dense = lists:reverse(UnreversedArray)}, Index, Data};

decode_amf3_array(dense, Size, Data, Index0, Acc0) when Size > 0 ->
    {Value, Rest, Index1} = decode_amf3_value(Data, Index0),
    Acc = Acc0#amf3_array{dense = [Value] ++ Acc0#amf3_array.dense},
    decode_amf3_array(dense, Size - 1, Rest, Index1, Acc).

decode_amf3_object_values(Traits, Data, Index) ->
    lists:foldl(fun(Trait, Acc) ->
        {Object, Rest0, Index0} = Acc,
        {Value, Index1, Rest1} = decode_amf3_value(Rest0, Index0),
        NewObject = Object#{Trait => Value},
        {NewObject, Rest1, Index1}
        end, {#{}, Data, Index}, Traits).

decode_amf3_object_traits(Size, Data, Index) when Size > 0 ->
    decode_amf3_object_traits(Size, Data, Index, []).

decode_amf3_object_traits(0, Data, Index, Acc) ->
    Return = lists:reverse(Acc),
    {Return, Data, Index};

decode_amf3_object_traits(Size, Data, Index0, Acc0) ->
    {UTF8VR, Rest0} = decode_amf3_u29(Data),
    LengthRef = UTF8VR bsr 1,
    if
        (LengthRef rem 2) =:= 0 ->
            Value = get_amf3_reference(string, LengthRef, Index0),
            Acc1 = [Value] ++ Acc0,
            decode_amf3_object_traits(Size - 1, Rest0, Index0, Acc1);
        true ->
            <<Value:LengthRef/binary, Rest1/binary>> = Rest0,
            Index1 = add_amf3_reference(string, Value, Index0),
            Acc1 = [Value ++ Acc0],
            decode_amf3_object_traits(Size - 1, Rest1, Index1, Acc1)
    end.

decode_amf3_object_dynamics(Data, Index) ->
    decode_amf3_object_dynamics(Data, Index, #{}).

decode_amf3_object_dynamics(<<16#0101:16/integer, Rest/binary>>, Index, Acc) ->
    {Acc, Rest, Index};

decode_amf3_object_dynamics(Data, Index0, Acc0) ->
    {UTF8VR, Rest0} = decode_amf3_u29(Data),
    LengthRef = UTF8VR bsr 1,
    {Key, Rest1, Index1} = if
        (LengthRef rem 2) =:=0 ->
            {get_amf3_reference(string, LengthRef, Index0), Rest0, Index0};
        true ->
            <<IfKey:LengthRef/binary, IfRest/binary>> = Rest0,
            IfIndex = add_amf3_reference(string, IfKey, Index0),
            {IfKey, IfRest, IfIndex}
    end,
    {Value, Rest2, Index2} = decode_amf3_value(Rest1, Index1),
    Acc1 = Acc0#{Key => Value},
    decode_amf3_object_dynamics(Rest2, Index2, Acc1).
