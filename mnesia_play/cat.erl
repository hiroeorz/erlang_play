-module(cat).

-include_lib("stdlib/include/qlc.hrl").
-record(cat, {name, type, old}).

-compile(export_all).


setup_schema() ->
    mnesia:create_schema(['erl@ceres.komatsuelec.co.jp']).

create_table() ->
    mnesia:create_table(cat, [{attributes, record_info(fields, cat)}]).

insert(Name, Type, Old) ->
    F = fun() ->
		mnesia:write(#cat {name = Name, type = Type, old = Old})
	end,
    
    mnesia:transaction(F).

all_cats() ->
    do(qlc:q([X#cat.name || X <- mnesia:table(cat)])).

adult_cats() ->
    do(qlc:q([X#cat.name || X <- mnesia:table(cat),
			   X#cat.old > 6])).

child_cats() ->
    do(qlc:q([X#cat.name || X <- mnesia:table(cat),
			   X#cat.old < 3])).

get(Name) ->
    F = fun() ->
		mnesia:read({cat, Name})
	end,
    
    {atomic, [Result]} = mnesia:transaction(F),
    Result.

do(Q) ->
    F = fun() ->
		qlc:e(Q) end,

    {atomic, Val} = mnesia:transaction(F),
    Val.
