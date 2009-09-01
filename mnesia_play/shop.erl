-module(shop).
-record(shop, {item, quantity, cost}).
-export([schema/0, start/0, create/0, insert/0, show/0]).

schema() ->
    mnesia:create_schema(['erl@ceres.komatsuelec.co.jp', 
			  'erl@eris.komatsuelec.co.jp']).

start() ->
    mnesia:start(),
    rpc:call('erl@eris.komatsuelec.co.jp', mnesia, start, []).

create() ->
    mnesia:create_table(shop, [{attributes, record_info(fields, shop)}]).

insert() ->
    F = fun() ->
		mnesia:write(#shop {item="apple", quantity=20, cost=2.3}),
		mnesia:write(#shop {item="orange", quantity=30, cost=3.5})
	end,

    mnesia:transaction(F).

show() ->
    F = fun() ->
		lists:foreach(
		  fun(X) ->
			  io:format("~p~n", [mnesia:read(shop, X, read)])
		  end,
		  mnesia:all_keys(shop))
	end,
    
    mnesia:transaction(F).
	
