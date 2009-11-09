-module(calc_clusters).
-compile(export_all).
-import(lists).

init() ->
    case ets:info(clusters) of
	undefined ->
	    ets:new(clusters, [private, named_table, bag]);
	_ -> none_exec
    end.


spawn_clusters(NodesList, ProcessCount) ->
    Results = connect(NodesList),
    
    F = fun(Result) ->
		{Node, Bool} = Result,

		case Bool of
		    true -> spawn_clusters_of_node(Node, ProcessCount); 
		    _ -> not_connected
		end
	end,

    [F(Result) || Result <- Results].

spawn_clusters_of_node(Node, ProcessCount) ->
    case ProcessCount of
	0 -> ok;
	_ ->
	    start(Node),
	    spawn_clusters_of_node(Node, ProcessCount - 1)
    end.    

connect(Nodes) ->
    Results = [{Node, ping(Node)} || Node <- Nodes],
    shell_default:nl(?MODULE),
    Results.

%%リモートでプロセスを立ち上げる。
start(Node) ->
    %init(),
    %ping(Node),                             
    %shell_default:nl(?MODULE),              

    Pid = spawn_link(Node, ?MODULE, loop, []),

    ets:insert(clusters, {?MODULE, Pid}),
    Pid.


%%start/1の名前登録版。他のホストのノードからも登録した名前で処理を依頼できるようになる。
start(Node, Name) ->
    global:register_name(Name, start(Node)).

%% リモートに処理を依頼。
%%   Pidが登録された名前でもPid番号でも同様に動作するように。
rpc(Pid, Request) ->
    case is_atom(Pid) of
	true -> global:send(Pid, {self(), Pid, Request});
	false -> Pid ! {self(), Pid, Request}
    end,
    
    receive
	{Pid, Response} -> Response;
	Any -> io:format("received: ~p~n", [Any])

    after 1000 ->
	    io:format("timeout~n"),

	    Pids = ets:lookup(clusters, ?MODULE),
	    NewPids = lists:delete({?MODULE, Pid}, Pids),

	    ets:delete(clusters, ?MODULE),
	    [ets:insert(clusters, X) || X <- NewPids],
	    
	    io:format("cluster deleted from table: ~p~n", [Pid]),
	    
	    case NewPids of 
		[] -> non_clusters;
		_ -> rpc(Request)
	    end
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
