#!/usr/bin/env escript

main(A) ->
    [Str] = A,
    send_server(Str).

send_server(Str) ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345, [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, Str),
    receive_data(Socket).

receive_data(Socket) ->
    receive
	{tcp, Socket, Str} ->
	    io:format("received_data: ~s~n", [Str]),
	    gen_tcp:close(Socket)
    end.

