-record(handshake_info, {
    socket :: gen_tcp:socket(),
    server_epoch :: pos_integer(),
    client_epoch :: pos_integer()
    }).