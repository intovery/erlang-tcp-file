%%%-------------------------------------------------------------------
%% @doc tcp_01 public API
%% @end
%%%-------------------------------------------------------------------

-module(tcp_01_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, [Port]) when is_integer(Port) ->
    io:format("~p~n", [Port]),
    _Pid = spawn_link(echo, accept, [Port]),
    tcp_01_sup:start_link();

start(_startType, WrongArgs) ->
    io:format("<system> tcp_01_app:start/2 failed. wrong args. args:~p~n",[WrongArgs]),
    {error, wrong_args}.

stop(_State) ->
    ok.

%% internal functions
