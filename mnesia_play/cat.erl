-module(cat).
-include_lib("stdlib/include/qlc.hrl").
-compile(export_all).

-record(cat, {name, type, old}).


setup_schema() ->
    mnesia:create_schema(['erl@ceres.komatsuelec.co.jp']).

start() ->
    mnesia:start().

create_table() ->
    mnesia:create_table(cat, [{attributes, record_info(fields, cat)}]).

insert(Name, Type, Old) ->
    F = fun() ->
		mnesia:write(#cat {name = Name, type = Type, old = Old})
	end,
    
    mnesia:transaction(F).

get(Name) ->
    F = fun() ->
		mnesia:read({cat, Name})
	end,
    
    {atomic, [Result]} = mnesia:transaction(F),
    Result.

all() ->
    do(qlc:q([{X#cat.name, X#cat.type, X#cat.old} || X <- mnesia:table(cat)])).

adult_cats() ->
    do(qlc:q([X#cat.name || X <- mnesia:table(cat),
			   X#cat.old > 6])).

child_cats() ->
    do(qlc:q([X#cat.name || X <- mnesia:table(cat),
			   X#cat.old < 3])).

do(Q) ->
    F = fun() ->
		qlc:e(Q) end,

    {atomic, Val} = mnesia:transaction(F),
    Val.
