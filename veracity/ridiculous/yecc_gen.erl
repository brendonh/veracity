%%%-------------------------------------------------------------------
%%% File    : yecc_gen.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Generate yecc files from ABNF ASTs
%%%
%%% Created : 25 Dec 2008 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(yecc_gen).

-export([generate/1, test/0]).


test() ->
    {ok, AST} = abnf_parse:test(),
    generate(AST).


generate(AST) ->
    put(yecc_counter, 0),
    Rules = do_rules(AST),
    io:format(lists:flatten(Rules)).


do_rules({rules, Rules}) ->
    io:format("Doing rules~n"),
    [do_rule(R) || R <- Rules].


do_rule({rule, {rulename, RuleName}, RuleBits}) ->
    {Bits, NewRules} = do_bits(RuleBits),
    [io_lib:format("~n~s -> ~p : '$1'.~n", [RuleName, Bits])|NewRules].

do_bits({range, From, To}) ->
    Rulename = get_rulename(),
    {Rulename, [io_lib:format("~s -> ~p : ~p.~n", [Rulename, I, I])
                || I <- lists:seq(From, To)]};

do_bits({concatenation, Alts}) ->
    {NewRules, Bits} = lists:foldl(
                         fun(E, {Rules, Bits}) -> 
                                 {NewBits, NewRules} = do_bits(E),
                                 {[NewRules|Rules], [NewBits," "|Bits]}
                         end, {[], []}, Alts),
    {lists:flatten(tl(lists:reverse(Bits))), lists:reverse(NewRules)};

do_bits({alternation, Alts}) ->
    Rulename = get_rulename(),

    NewRules = lists:map(
                 fun(E) -> 
                         {BitRulename, BitRules} = do_bits(E),
                         BitRule = io_lib:format("~s -> ~p : '$1'~n", [Rulename, BitRulename]),
                         [BitRule,BitRules]
                 end, Alts),
    {Rulename, NewRules};

do_bits({{repeat, From, To}, Bit}) when is_list(From) and is_list(To) ->
    IFrom = list_to_integer(From),
    ITo = list_to_integer(To),
    {BitRulename, BitRules} = do_bits(Bit),
    Rulename = get_rulename(),
    SpaceName = " " ++ BitRulename,
    MRN = fun(X) -> tl(string:copies(SpaceName, X)) end,
    MRP = fun(X) -> lists:flatten([$[, 
                                   tl(lists:flatten([[",'\$", integer_to_list(I), "'"]
                                                     || I <- lists:seq(1, X)])),
                                   $]]) end,
    MR = fun(X) -> io_lib:format("~s -> ~s : ~s.~n", [Rulename, MRN(X), MRP(X)]) end,

    Rules = [lists:flatten(MR(X)) || X <- lists:seq(IFrom, ITo)],
    {Rulename, Rules ++ BitRules};
                  

do_bits({rulename, RuleName}) ->
    {RuleName, []};

do_bits({char, [C]}) ->
    {C, []};

do_bits(Bits) ->
    {Bits, []}.

get_rulename() ->   
    ID = get(yecc_counter),
    put(yecc_counter, ID+1),
    "Rule" ++ integer_to_list(ID).
