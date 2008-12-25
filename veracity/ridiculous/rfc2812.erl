%%%-------------------------------------------------------------------
%%% File    : rfc2812.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : IRC message parsing
%%%
%%% Created : 25 Dec 2008 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(rfc2812).

-export([test/0]).


scan_string(String) ->
    [classify(O) || O <- String].

classify(C) when C >= 16#41, C =< 16#5A -> {letter, C};
classify(C) when C >= 16#61, C =< 16#7A -> {letter, C};
classify(C) when C >= 16#30, C =< 16#39 -> {digit, C};
classify(C) when C >= 16#5B, C =< 16#60 -> {special, C};
classify(C) when C >= 16#7B, C =< 16#7D -> {special, C};
classify(C) -> {octet, C}.


test() ->
    io:format("~p~n", [scan_string("Hello {Brendon}!")]).
     
    



