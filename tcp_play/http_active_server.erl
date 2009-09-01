-module(http_active_server).
-export([start/0, start/1]).

start() ->
    start(2345).

start(PORT) ->
    io:format("started server at active mode.~n"),
    {ok, Listen} = gen_tcp:listen(PORT, [binary, 
					 {packet, 0}, 
					 {reuseaddr, true},
					 {active, true}]),
    wait_client(Listen).

wait_client(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),

    Pid = spawn(fun() -> child_process(Socket) end),
    gen_tcp:controlling_process(Socket, Pid),

    wait_client(Listen).

child_process(Socket) ->
    inet:setopts(Socket, [{packet, 0}, 
			  binary, 
			  {nodelay, true}, 
			  {active, true}]),

    receive
	{tcp, Socket, Str} ->
	    io:format("received_data: ~s~n", [Str]),

	    gen_tcp:send(Socket, header() ++ document()),
	    gen_tcp:close(Socket),

	    io:format("socket closed~n")
    end.

header() ->
    "HTTP/1.0 200 OK\r\n" ++
	"Cache-Control:private\r\n" ++
	"Content-Type: text/html\r\n" ++
	"Connection:Close \r\n\r\n".

document() ->
    {ok, Data} = file:read_file("./sample_document.html"),
    Data.
