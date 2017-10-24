-module(cowboy_php_hello_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, _} = cowboy_php_hello:start_with_phpfpm(33080, 33000),
    cowboy_php_hello_sup:start_link().

stop(_State) ->
    cowboy_php_hello:stop().
