%%%-------------------------------------------------------------------
%% @doc tcp_01 public API
%% @end
%%%-------------------------------------------------------------------

-module(tcp_01_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    _Pid = spawn_link(echo, accept, [28201]),
    tcp_01_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
