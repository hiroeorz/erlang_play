-module(gcalc).
-compile(export_all).

%%リモートでプロセスを立ち上げる。
start(Node) ->
    ping(Node),                       %%リモートのノードと接続を確立する。
    shell_default:nl(?MODULE),        %%モジュールをリモートのノードに送信する。
    spawn(Node, fun() -> loop() end). %%ノードを指定してプロセスを立ち上げ。

%%start/1の名前登録版。他のホストのノードからも登録した名前で処理を依頼できるようになる。
start(Node, Name) ->
    global:register_name(Name, start(Node)).

rpc(Pid, Request) ->
    %% Pidが登録された名前でもPid番号でも同様に動作するように。
    case is_atom(Pid) of
	true -> global:send(Pid, {self(), Pid, Request});
	false -> Pid ! {self(), Pid, Request}
    end,
    
    %%リモートからの処理結果を受信する。
    receive
	{Pid, Response} -> Response;
	Any -> io:format("received: ~p~n", [Any])
    end.

%%リモートとの接続を確立。
ping(Node) ->
    net_kernel:connect_node(Node).

%%リモートの処理リクエスト待ちのループ。
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

%%足算するだけの関数。
plus(X, Y) ->
    X + Y.

%%引算するだけの関数。
minus(X, Y) ->
    X - Y.
