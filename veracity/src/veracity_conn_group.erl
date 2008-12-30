%%%-------------------------------------------------------------------
%%% File    : veracity_conn_group.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Manage processes for one connection
%%%
%%% Created : 31 Dec 2008 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(veracity_conn_group).

-behaviour(supervisor).

%% API
-export([start_link/1, get_child/2]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link(Opts) ->
    supervisor:start_link(?MODULE, Opts).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(Opts0) ->

    Opts = [{supervisor, self()}|Opts0],

    Conn = {conn,{veracity_conn,start_link,[Opts]},
            permanent,2000,worker,[veracity_conn]},

    Users = {users, {veracity_users, start_link, [Opts]},
             permanent,2000, worker, [veracity_users]},

    {ok,{{one_for_one,1,1}, [Conn, Users]}}.

%%====================================================================
%% Internal functions
%%====================================================================

get_child(SupRef, ID) ->
    hd([Child || {CID, Child, _, _} <- supervisor:which_children(SupRef),
                 CID == ID]).
