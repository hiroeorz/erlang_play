-module(gcalc).
-compile(export_all).
-import(lists).

init() ->
    case ets:info(clusters) of
	undefined ->
	    ets:new(clusters, [private, named_table, bag]);
	_ -> none_exec
    end.


%%リモートでプロセスを立ち上げる。
start(Node) ->
    init(),

    ping(Node),                             %%リモートのノードと接続を確立する。
    shell_default:nl(?MODULE),              %%モジュールをリモートのノードに送信する。

    Pid = spawn(Node, fun() -> loop() end), %%ノードを指定してプロセスを立ち上げ。

    ets:insert(clusters, {?MODULE, Pid}),   %%Pid番号を保存しておく。
    Pid.


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

rpc(Request) ->
    Pid = get_pid(),
    rpc(Pid, Request).

%%リモートとの接続を確立。
ping(Node) ->
    net_kernel:connect_node(Node).

get_pid() ->
    case ets:lookup(clusters, ?MODULE) of
	[] -> none_clusters;

	[{?MODULE, Pid}] -> 
	    io:format("calling to ~p~n", [Pid]),
	    Pid;
	Clusters ->
	    Index = random:uniform(length(Clusters)),
	    {?MODULE, Pid} = lists:nth(Index, Clusters),
	    io:format("calling to ~p~n", [Pid]),
	    Pid
    end.
    

%%リモートの処理リクエスト待ちのループ。
loop() ->
    receive
	{From, Name, {plus, X, Y}} ->
	    From ! {Name, remote_plus(X, Y)},
	    loop();

	{From, Name, {minus, X, Y}} ->
	    From ! {Name, remote_minus(X, Y)},
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
remote_plus(X, Y) ->
    X + Y.

plus(X, Y) ->
    rpc({plus, X, Y}).

%%引算するだけの関数。
remote_minus(X, Y) ->
    X - Y.

minus(X, Y) ->
    rpc({minus, X, Y}).
