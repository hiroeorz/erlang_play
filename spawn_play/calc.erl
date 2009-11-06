-module(calc).
-compile(export_all).

start() ->
    spawn(fun() -> loop() end).

start(Name) ->
    register(Name, start()).

rpc(Pid, Request) ->
    Pid ! {self(), Pid, Request},

    receive
	{Pid, Response} ->
	    Response
    end.

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
