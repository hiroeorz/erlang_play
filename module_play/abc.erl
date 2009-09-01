-module(abc).
-export([a/2, b/1]).

a(X) ->
    X + 1.

a(X, Y) ->
    X + Y.

b(X) ->
    X - 1.

b(X, Y) ->
    X - Y.
     
