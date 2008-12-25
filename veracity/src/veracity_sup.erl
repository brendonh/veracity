%%%-------------------------------------------------------------------
%%% File    : veracity_sup.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 25 Dec 2008 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(veracity_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).


%%====================================================================
%% Supervisor callbacks
%%====================================================================
init(_Args) ->
    {ok,{{one_for_one,0,1}, []}}.


%%====================================================================
%% Internal functions
%%====================================================================
