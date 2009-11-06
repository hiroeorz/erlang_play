-module(gcalc).
-compile(export_all).

start(Node) ->
    ping(Node),
    shell_default:nl(?MODULE),

    spawn(Node, fun() -> loop() end).

start(Node, Name) ->
    global:register_name(Name, start(Node)).

rpc(Pid, Request) ->
    global:send(Pid, {self(), Pid, Request}),
    
    receive
	{Pid, Response} -> Response;
	Any -> io:format("received: ~p~n", [Any])
    end.

ping(Node) ->
    net_kernel:connect_node(Node).

loop() ->
    receive
	{From, Name, {plus, X, Y}} ->
	    From ! {Name, plus(X, Y)},
	    loop();

	{From, Name, {minus, X, Y}} ->
	    From ! {Name, minus(X, Y)},
	    loop();

	{From, Name, Any} ->
	    io:format("received: ~p~n", [Any]),
	    From ! {Name, unknown},
	    loop();

	Any ->
	    io:format("received: ~p~n", [Any]),
	    loop()
    end.

plus(X, Y) ->
    X + Y.

minus(X, Y) ->
    X - Y.
