%%%-------------------------------------------------------------------
%%% File    : veracity_conn_sup.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : simple_one_for_one connection supervisor
%%%
%%% Created : 29 Dec 2008 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(veracity_conn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, connect/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

connect(Opts) ->
    {ok, Pid} = supervisor:start_child(veracity_conn_sup, [Opts]),
    Conn = veracity_conn_group:get_child(Pid, conn),
    Users = veracity_conn_group:get_child(Pid, users),
    {Conn, Users}.


%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->

    GroupSpec = {group ,{veracity_conn_group,start_link,[]},
                 transient,2000,supervisor,[veracity_conn_group]},

    {ok,{{simple_one_for_one,0,1}, [GroupSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
