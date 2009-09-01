#!/usr/bin/env escript

main(_) ->
    start_simple_server().

start_simple_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, 
					 {packet, 4}, 
					 {reuseaddr, true},
					 {active, true}]),
    start_accept(Listen).

start_accept(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    server_loop(Socket),
    start_accept(Listen).

server_loop(Socket) ->
    receive
	{tcp, Socket, Str} ->
	    io:format("received_data: ~s~n", [Str]),
	    gen_tcp:send(Socket, "OK Received."),
	    server_loop(Socket);
	{tcp_closed, Socket} ->
	    io:format("socket closed~n"),
	    gen_tcp:close(Socket)
    end.
