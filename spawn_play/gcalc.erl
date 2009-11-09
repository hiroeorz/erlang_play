-module(gcalc).
-compile(export_all).

start(Node) ->
    ping(Node),                %%リモートのノードと接続を確立する。
    shell_default:nl(?MODULE), %%モジュールをリモートのノードに送信する。
    spawn(Node, fun() -> loop() end).

start(Node, Name) ->
    global:register_name(Name, start(Node)).

rpc(Pid, Request) ->

    %% Pidが登録された名前でもPid番号でも同様に動作するように。
    case is_atom(Pid) of
	true -> global:send(Pid, {self(), Pid, Request});
	false -> Pid ! {self(), Pid, Request}
    end,
    
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
