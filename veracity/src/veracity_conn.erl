%%%-------------------------------------------------------------------
%%% File    : veracity_conn.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : IRC server connection
%%%
%%% Created : 29 Dec 2008 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(veracity_conn).

-behaviour(gen_server).

-include("veracity_util.hrl").

%% API
-export([start_link/1, space_join/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  opts,
  socket,
  listeners,
  loggers,
  ready
}).

-define(NEXT(S), inet:setopts(S#state.socket, [{active, once}])).


%%====================================================================
%% API
%%====================================================================

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->

    {ok, Sock} = gen_tcp:connect(
                   ?GVD(server, Opts, -1), %% Crash on no server
                   ?GVD(port, Opts, 6667),
                   [{active, once},
                    {packet, line},
                    {reuseaddr, true}]),

    ?DBG({conn, Sock}),

    State = #state{opts=Opts, 
                   socket=Sock,
                   listeners=[],
                   loggers=?GVD(loggers, Opts, []),
                   ready=false},

    send_connect_messages(State),

    {ok, State}.


               
handle_call({listen_msg, {Prefix, Target, Msg}}, {Pid, _Ref}, State) ->
    ?DBG({listen_msg, {Prefix, Target, Msg}}),
    {NewState, Reply} = add_filter(State, Pid, [privmsg, Prefix, Target, Msg]),
    {reply, Reply, NewState};

handle_call({listen, {Type, Prefix}}, {Pid, _Ref}, State) ->
    ?DBG({listen, {Type, Prefix}}),
    {NewState, Reply} = add_filter(State, Pid, [Type, Prefix]),
    {reply, Reply, NewState};

handle_call(Request, _From, State) ->
    ?DBG({unknown_call, Request}),
    {reply, ok, State}.


handle_cast({send, Args}, State) ->
    send(State, Args),
    {noreply, State};

handle_cast(Msg, State) ->
    ?DBG({unknown_cast, Msg}),
    {noreply, State}.



handle_info({tcp_closed, _Sock}, State) ->
    logcast(State, recv, disconnected),
    %broadcast(State, disconnected),

    Reason = case ?GVD(reconnect, State#state.opts, false) of
                 true -> disconnected;
                 false -> normal
             end,
    
    {stop, Reason, State};

handle_info({tcp, _Sock, Message}, State) ->
    
    logcast(State, recv, Message),

    %?DBG({msg, Message}),
    %?DBG({parse, (catch veracity_parse:message(chomp(Message)))}),

    case (catch veracity_parse:message(chomp(Message))) of
        {ok, {Prefix, RawCommand, Params}} ->
            Command = translate(RawCommand),
            NewState = handle(State, {Prefix, Command, Params});
        Other ->
            ?DBG({parse_error, Message, Other}),
            NewState = State
    end,

    ?NEXT(NewState),
    {noreply, NewState};

handle_info(Info, State) ->
    ?DBG({unknown_info, Info}),
    ?NEXT(State),
    {noreply, State}.



terminate(_Reason, _State) ->
    ?DBG(terminating),
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

add_filter(State, Pid, FilterSpec) ->
    Listeners = State#state.listeners,
    Filter = [make_filter(X) || X <- FilterSpec],

    case lists:any(fun(X) -> X == error end, Filter) of
        false -> 
            {State#state{listeners=[{Filter, Pid}|Listeners]}, ok};
        true -> {State, parse_error}
    end.


make_filter(all) -> fun(_) -> true end;
make_filter({S, O}) ->
    case re:compile(S, O) of
        {ok, R} -> fun(M) -> re:run(M, R) /= nomatch end;
        {error, _} -> error
    end;
make_filter(A) when is_atom(A) ->
    fun(X) -> X == A end;
make_filter(F) when is_function(F) ->
    F;
make_filter(S) -> 
    case re:compile(S) of
        {ok, R} -> fun(M) -> re:run(M, R) /= nomatch end;
        {error, _} -> error
    end.


translate([_,_,_]=Maybe3Digit) ->
    Parsed = case veracity_parse:digit(Maybe3Digit) of
        true -> list_to_integer(Maybe3Digit);
        false -> Maybe3Digit
    end,
    veracity_codes:code(Parsed);
translate(Other) -> veracity_codes:code(Other).


handle(State, {_, rpl_welcome, _}) ->
    State#state{ready=true};

handle(State, {Prefix, Type, [Target,Msg]}) ->
    Message = [Type, Prefix, Target, Msg],
    Matches = lists:usort([Listener || {Spec, Listener} <- State#state.listeners,
                                       listen_match(Spec, Message)]),
    [M ! {Type, {Prefix, Target, Msg}} || M <- Matches, M /= none],
    State;

handle(State, {Prefix, Type, Args}) ->
    Message = [Type, Prefix],
    Matches = lists:usort([Listener || {Spec, Listener} <- State#state.listeners,
                                       listen_match(Spec, Message)]),
    [M ! {Type, {Prefix, Args}} || M <- Matches, M /= none],
    State;

handle(State, Other) -> 
    ?DBG({ignoring, Other}),
    State.


send_connect_messages(State) ->
    Opts = State#state.opts,

    case ?GV(pass, Opts) of
        undefined -> ok;
        Pass -> send(State, ["PASS ", Pass])
    end,
    
    Nick = ?GVD(nick, Opts, "verac"),
    send(State, ["NICK ", Nick]),
    
    Username = ?GVD(username, Opts, "veracity"),
    Realname = ?GVD(realname, Opts, "veracity"),
    
    Wallops = ?GVD(wallops, Opts, false),
    Invisible = ?GVD(invisible, Opts, false),
    
    Mask = case {Wallops, Invisible} of
               {false, false} -> "0";
               {true, false} -> "4";
               {false, true} -> "8";
               {true, true} -> "12"
           end,
    
    UserCmd = space_join(["USER", Username, Mask, "*", ":" ++ Realname]),

    send(State, UserCmd).

space_join(Stuff) ->
    Prefixed = lists:foldl(fun(E, A) -> [" ",E|A] end, [], Stuff),
    lists:reverse(tl(Prefixed)).

send(State, Bits) ->
    Msg = lists:concat(Bits ++ ["\r\n"]),
    logcast(State, send, Msg),
    gen_tcp:send(State#state.socket, Msg).


listen_match(Filter, Msg) when length(Filter) == length(Msg) ->
    lists:all(fun({F,M}) -> F(M) end, 
              lists:zip(Filter, Msg));
listen_match(_Filter, _Msg) -> false.
            

logcast(State, Type, Term) ->
    [L ! {Type, Term} || L <- State#state.loggers].


chomp(S) ->
    string:strip(string:strip(S, right, $\n), right, $\r).

