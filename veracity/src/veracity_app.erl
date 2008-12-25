%%%-------------------------------------------------------------------
%%% File    : veracity_app.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : IRC!
%%%
%%% Created : 25 Dec 2008 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(veracity_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([start/0]).


%%====================================================================
%% Application callbacks
%%====================================================================
start(_Type, StartArgs) ->
    veracity_sup:start_link(StartArgs).

start() ->
    application:start(veracity).

stop(_State) ->
    ok.


%%====================================================================
%% Internal functions
%%====================================================================
