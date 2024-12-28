%%%-------------------------------------------------------------------
%% @doc fp2 public API
%% @end
%%%-------------------------------------------------------------------

-module(fp2_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    fp2_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
