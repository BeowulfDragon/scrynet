-module(echo).

-export[init/1, recieve/2].

init(ConnectionRef) ->
    io:fwrite(ConnectionRef),
    {}.

recieve(Data, _State) ->
    io:fwrite(Data),
    {}.