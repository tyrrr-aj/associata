%%%-------------------------------------------------------------------
%% @doc aas public API
%% @end
%%%-------------------------------------------------------------------

-module(aas_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    
    aas_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
