-module(mysql_pool_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([init_pool/0]).
-export([init_pool/2]).

start(_Type, _Args) ->
	mysql_pool_sup:start_link().

stop(_State) ->
	ok.

init_pool() ->
	{ok, Pools} = application:get_env(mysql_pool, pools),
	lists:map(fun({PoolArgs, WorkerArgs}) -> init_pool(PoolArgs, WorkerArgs) end, Pools).

init_pool(PoolArgs, WorkerArgs) ->
	pool_sup:start_child([PoolArgs, WorkerArgs]).