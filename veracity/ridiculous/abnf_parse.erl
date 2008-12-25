%%%-------------------------------------------------------------------
%%% File    : abnf_parse.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Parse ABNF grammar specs
%%%
%%% Created : 25 Dec 2008 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(abnf_parse).

-export([parse/1, test/0]).

load(Filename) ->
    {ok, Content} = file:read_file(Filename),
    Stripped = strip_comments(binary_to_list(Content)),
    Stripped2 = strip_whitespace(Stripped),
    compress_empty_lines(Stripped2).

parse(Filename) ->
    Content = load(Filename),
    abnf:parse([{X, 1} || X <- Content]).

test() ->
    parse("rfc2812.abnf").


strip_comments(Str) ->
    strip_comments(Str, [], false).

strip_comments([$;|Rest], Buf, _) ->
    strip_comments(Rest, Buf, true);
strip_comments([$\n|Rest], Buf, true) ->
    strip_comments(Rest, [$\n|Buf], false);
strip_comments([_|Rest], Buf, true) ->
    strip_comments(Rest, Buf, true);
strip_comments([C|Rest], Buf, false) ->
    strip_comments(Rest, [C|Buf], false);
strip_comments([], Buf, _) ->
    lists:reverse(Buf).


strip_whitespace(Str) ->
    strip_whitespace(Str, []).

strip_whitespace([$\s,$\s|Rest], Buf) ->
    strip_whitespace(Rest, Buf);
strip_whitespace([$\s,$\n|Rest], Buf) ->
    strip_whitespace(Rest, [$\n|Buf]);
strip_whitespace([$\s,$/|Rest], Buf) ->
    strip_whitespace([$/|Rest], Buf);

strip_whitespace([$\s,$=|Rest], Buf) ->
    strip_whitespace([$=|Rest], Buf);
strip_whitespace([$=,$\s|Rest], Buf) ->
    strip_whitespace([$=|Rest], Buf);

strip_whitespace([$(,$\s|Rest], Buf) ->
    strip_whitespace([$(|Rest], Buf);
strip_whitespace([$\s,$)|Rest], Buf) ->
    strip_whitespace([$)|Rest], Buf);

strip_whitespace([$[,$\s|Rest], Buf) ->
    strip_whitespace([$[|Rest], Buf);
strip_whitespace([$\s,$]|Rest], Buf) ->
    strip_whitespace([$]|Rest], Buf);

strip_whitespace([$/,$\s|Rest], Buf) ->
    strip_whitespace(Rest, [$/|Buf]);
strip_whitespace([C|Rest], Buf) ->
    strip_whitespace(Rest, [C|Buf]);
strip_whitespace([], Buf) ->
    lists:reverse(Buf).


compress_empty_lines(Str) ->
    compress_empty_lines(Str, []).

compress_empty_lines([$\n,$\n|Rest], Buf) ->
    compress_empty_lines([$\n|Rest], Buf);
compress_empty_lines([$\n], Buf) ->
    lists:reverse(Buf);
compress_empty_lines([C|Rest], Buf) ->
    compress_empty_lines(Rest, [C|Buf]);
compress_empty_lines([], Buf) ->
    lists:reverse(Buf).
