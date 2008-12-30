%%%-------------------------------------------------------------------
%%% File    : vtest.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Veracity test sandbox
%%%
%%% Created : 29 Dec 2008 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(vtest).

-export([test/0]).

-include("veracity_util.hrl").

test() ->
    spawn_link(fun go/0).


go() ->
    {Conn, Users} = veracity_conn_sup:connect([{server, "irc.freenode.net"},
                                           {pass, "arthur"},
                                           {listeners, []},
                                           {loggers, [self()]}]),
    
    ?DBG({conn, Conn, users, Users}),

    gen_server:call(Conn, {listen_msg, {all, all, "(?i)w"}}),

    spawn(fun() -> go2(Conn) end),

    gen_server:cast(Conn, {send, ["WHOIS brend"]}),

    loop().

loop() ->
    receive
        {send, S} -> io:format("~p: SEND: ~s", [self(), S]);
        {recv, S} -> io:format("~p: RECV: ~s", [self(), S]);
        {privmsg, Msg} -> io:format("~p: PRIV: ~p~n", [self(), Msg])
    end,
    loop().
             

go2(Pid) ->
    gen_server:call(Pid, {listen_msg, {"^(?i).*b.*!.*", all, all}}),
    loop().
