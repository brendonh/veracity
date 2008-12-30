%%%-------------------------------------------------------------------
%%% File    : veracity_users.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Track users
%%%
%%% Created : 31 Dec 2008 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(veracity_users).

-behaviour(gen_server).

-include("veracity_util.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
 opts,
 conn
}).

%%====================================================================
%% API
%%====================================================================
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================


init(Opts) ->
    gen_server:cast(self(), findconn),
    {ok, #state{opts=Opts}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(findconn, State) ->
    Sup = ?GV(supervisor, State#state.opts),
    Conn = veracity_conn_group:get_child(Sup, conn),

    Whois = fun(T) ->
                    lists:any(fun(X) -> X == T end,
                              [rpl_whoisuser, rpl_whoisserver,
                               rpl_whoisoperator, rpl_whoisidle,
                               rpl_endofwhois, rpl_whoischannels,
                               rpl_whoislogin])
            end,

    gen_server:call(Conn, {listen, {Whois, all}}),
    {noreply, State#state{conn=Conn}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    ?DBG({user_info, Info}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
