%%%-------------------------------------------------------------------
%%% File    : veracity_parse.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Parse IRC bits.
%%%
%%% Created : 25 Dec 2008 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(veracity_parse).

-export([message/1, prefix/1, command/1, params/1, 
         nospcrlfcl/1, middle/1, trailing/1, space/1, crlf/1, 
         msgtarget/1, msgto/1, channel/1, 
         servername/1, host/1, hostname/1, shortname/1, 
         hostaddr/1, ip4addr/1, ip6addr/1, 
         nickname/1, targetmask/1, chanstring/1, channelid/1, 
         user/1, key/1, 
         letter/1, digit/1, hexdigit/1, special/1, octet/1,
         mask/1,

         test/0
]).

-define(NICK_LENGTH_LIMIT, 32).


%% message    =  [ ":" prefix SPACE ] command [ params ] crlf

message([$:|Rest]) ->
    {Tail, Prefix} = getnext(Rest),
    case prefix(Prefix) of
        true ->
            case message(tl(Tail)) of
                {ok, {none, Command, Params}} ->
                    {ok, {Prefix, Command, Params}};
                Other -> 
                    Other
            end;
        false ->
            {invalid_prefix, Prefix}
    end;
message(S) ->
    {Tail, Command} = getnext(S),
    case command(Command) of
        true  ->
            case params(Tail) of
                {ok, Params} -> {ok, {none, Command, Params}};
                {error, Error} -> Error
            end;
        false ->
            invalid_command
    end.
                   

%% ------------------------------------------------------------------ %%

%% prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )

prefix(S) ->
    servername(S) orelse nickuserhost(S).

nickuserhost(S) ->
    Bits = string:tokens(S, "@"),
    case length(Bits) of
        2 ->
            [NicknameUser, Host] = Bits,
            nicknameuser2(NicknameUser) andalso host(Host);
        1 -> nickname(S);
        _ -> false
    end.

nicknameuser2(S) ->
    Bits = string:tokens(S, "!"),
    case length(Bits) of
        2 ->
            [Nickname, User] = Bits,
            nickname(Nickname) andalso user(User);
        1 -> nickname(S);
        _ -> false
    end.


%% ------------------------------------------------------------------ %%

%% command    =  1*letter / 3digit

command(S) ->
    case length(S) of
        3 ->
            digit(S) orelse letter(S);
        0 -> invalid_command;
        _ -> letter(S)
    end.


%% ------------------------------------------------------------------ %%

%% params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
%%            =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]

params(S) ->
    RV = params(S, 0, []),
    case lists:member(invalid, RV) of
        true ->
            {error, invalid_params};
        false ->
            {ok, lists:reverse(RV)}
    end.

params([$\s,$:|Rest], _N, Acc) ->
    case trailing(Rest) of
        true -> [string:strip(Rest)|Acc];
        false -> [invalid|Acc]
    end;
params([$\s|Rest], N, Acc) when N < 14 ->
    {Tail, Middle} = getnext(Rest),
    case middle(Middle) of
        true -> params(Tail, N+1, [Middle|Acc]);
        false -> [invalid|Acc]
    end;
params([], N, Acc) when N < 14 ->
    Acc;
params(Rest, N, Acc) when N == 14 ->
    case trailing(Rest) of
        true -> [string:strip(Rest)|Acc];
        false -> [invalid|Acc]
    end;
params([], _, Acc) -> 
    [invalid|Acc];
params(What, Huh, _) ->
    io:format("What, huh?: ~p ~p~n", [What, Huh]),
    false.


getnext(S) ->
    getnext(S, []).

getnext([$\s|_R]=Inp, Buf) ->
    {Inp, lists:reverse(Buf)};
getnext([C|R], Buf) ->
    getnext(R, [C|Buf]);
getnext([], Buf) ->
    {[], lists:reverse(Buf)}.


%% middle     =  nospcrlfcl *( ":" / nospcrlfcl )

middle([C|R]) ->
    nospcrlfcl([C]) andalso tryfuncs(R, [fun nospcrlfcl/1, fun([$:]) -> true; (_) -> false end]).


%% trailing   =  *( ":" / " " / nospcrlfcl )

trailing(S) ->
    tryfuncs(S, [fun nospcrlfcl/1, fun ([$:]) -> true; ([$\s]) -> true; (_) -> false end]).
  

%% ------------------------------------------------------------------ %%

%% nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
%%                 ; any octet except NUL, CR, LF, " " and ":"

nospcrlfcl([C|R]) when C >= 16#01, C =< 16#09 -> nospcrlfcl(R);
nospcrlfcl([C|R]) when C >= 16#0B, C =< 16#0C -> nospcrlfcl(R);
nospcrlfcl([C|R]) when C >= 16#0E, C =< 16#1F -> nospcrlfcl(R);
nospcrlfcl([C|R]) when C >= 16#21, C =< 16#39 -> nospcrlfcl(R);
nospcrlfcl([C|R]) when C >= 16#3B, C =< 16#FF -> nospcrlfcl(R);
nospcrlfcl([]) -> true;
nospcrlfcl(_) -> false.


%% SPACE      =  %x20        ; space character

space([$\s]) -> true;
space(_) -> false.     


%% crlf       =  %x0D %x0A   ; "carriage return" "linefeed"

crlf([$\r,$\n]) -> true;
crlf(_) -> false.


%% ------------------------------------------------------------------ %%
    
%% target     =  nickname / server

%% target(S) ->
%%    nickname(S) orelse server(S).


%% msgtarget  =  msgto *( "," msgto )

msgtarget(S) ->
    Bits = string:tokens(S, ","),
    case lists:all(fun msgto/1, Bits) of
        true -> S;
        false -> invalid_msgtarget
    end.
        

%% msgto      =  channel / ( user [ "%" host ] "@" servername )
%% msgto      =/ ( user "%" host ) / targetmask
%% msgto      =/ nickname / ( nickname "!" user "@" host )

msgto(S) ->
    case channel(S) orelse msgto2(S)
        orelse msgto3(S) orelse targetmask(S)
        orelse nickname(S) orelse msgto4(S) of
        true -> true;
        false -> invalid_msgto
    end.
            

msgto2(S) ->
    Bits = string:tokens(S, "@"),
    case length(Bits) of
        2 ->
            [UserHost, ServerName] = Bits,
            userhost(UserHost) andalso servername(ServerName);
        _ -> false
    end.

userhost(S) ->
    Bits = string:tokens(S, "%"),
    case length(Bits) of
        1 -> user(Bits);
        2 ->
            [User, Host] = Bits,
            user(User) andalso host(Host);
        _ -> false
    end.


msgto3(S) ->
    Bits = string:tokens(S, "%"),
    case length(Bits) of
        2 ->
            [User, Host] = Bits,
            user(User) andalso host(Host);
        _ -> false
    end.

msgto4(S) ->
    Bits = string:tokens(S, "@"),
    case length(Bits) of
        2 ->
            [NicknameUser, Host] = Bits,
            nicknameuser(NicknameUser) andalso host(Host);
        _ -> false
    end.

nicknameuser(S) ->
    Bits = string:tokens(S, "!"),
    case length(Bits) of
        2 ->
            [Nickname, User] = Bits,
            nickname(Nickname) andalso user(User);
        _ -> false
    end.
            

%% ------------------------------------------------------------------ %%

% channel    =  ( "#" / "+" / ( "!" channelid ) / "&" ) chanstring
%                 [ ":" chanstring ]

channel([$#|S]) -> channel2(S);
channel([$+|S]) -> channel2(S);
channel([$&|S]) -> channel2(S);
channel([$!,A,B,C,D,E|S]) -> channelid([A,B,C,D,E]) andalso channel2(S);
channel(_) -> invalid_channel.

channel2(S) ->
    Bits = string:tokens(S, ":"),
    case length(Bits) of
        1 -> chanstring(S);
        2 -> [A,B] = Bits,
             chanstring(A) andalso chanstring(B);
        _ -> false
    end.
    

%% servername =  hostname

servername(S) -> hostname(S).     


%% host       =  hostname / hostaddr

host(S) -> hostname(S) orelse hostaddr(S) orelse cloak(S).


cloak(_S) ->
    true. % Heh heh

%% hostname   =  shortname *( "." shortname )

hostname([]) -> false;
hostname(S) ->
    Bits = string:tokens(S, "."),
    lists:all(fun shortname/1, Bits).
       

%% shortname  =  ( letter / digit ) *( letter / digit / "-" )
%%                *( letter / digit )
%%                  ; as specified in RFC 1123 [HNAME]
 
shortname([C]) ->
    tryfuncs(C, [fun letter/1, fun digit/1]);
shortname([C|R]) ->
    tryfuncs(C, [fun letter/1, fun digit/1])
        andalso tryfuncs(R, [fun letter/1, fun digit/1, fun([C2]) -> C2 == $- end])
        andalso not (lists:last(R) == $-).
                                                                 


%% hostaddr   =  ip4addr / ip6addr

hostaddr(S) -> ip4addr(S) orelse ip6addr(S).


%% ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit

ip4addr(S) ->
    Bits = string:tokens(S, "."),
    case length(Bits) of
        4 -> lists:all(fun(B) ->
                               L = length(B),
                               L >= 1 andalso L =< 3 andalso digit(B)
                       end, Bits);
        _ -> false
    end.

            
%% ip6addr    =  1*hexdigit 7( ":" 1*hexdigit )
%% ip6addr    =/ "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr
                  
ip6addr("0:0:0:0:0:0:" ++ IP) -> ip4addr(IP);
ip6addr("0:0:0:0:0:FFFF:" ++ IP) -> ip4addr(IP);
ip6addr(S) ->
    Bits = string:tokens(S, ":"),
    case length(Bits) of
        8 -> lists:all(fun hexdigit/1, Bits);
        _ -> false
    end.


%% nickname   =  ( letter / special ) *8( letter / digit / special / "-" )

nickname([C|R]) when length(R) =< ?NICK_LENGTH_LIMIT ->
    tryfuncs(C, [fun letter/1, fun special/1])
        andalso tryfuncs(R, [fun letter/1, fun digit/1, fun special/1,
                             fun([C2]) -> C2 == $- end]);
nickname(_) -> false.
     
                                
%% targetmask =  ( "$" / "#" ) mask
%%                ; see details on allowed masks in section 3.3.1

targetmask([$$|R]) -> mask(R);
targetmask([$#|R]) -> mask(R);
targetmask(_) -> false.
    

%% chanstring =  %x01-07 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B
%% chanstring =/ %x2D-39 / %x3B-FF
%%                ; any octet except NUL, BELL, CR, LF, " ", "," and ":"

chanstring([C|R]) when C >= 16#01, C =< 16#07 -> chanstring(R);
chanstring([C|R]) when C >= 16#08, C =< 16#09 -> chanstring(R);
chanstring([C|R]) when C >= 16#0B, C =< 16#0C -> chanstring(R);
chanstring([C|R]) when C >= 16#0E, C =< 16#1F -> chanstring(R);
chanstring([C|R]) when C >= 16#21, C =< 16#2B -> chanstring(R);
chanstring([C|R]) when C >= 16#2D, C =< 16#2D -> chanstring(R);
chanstring([C|R]) when C >= 16#3B, C =< 16#FF -> chanstring(R);
chanstring([]) -> true;
chanstring(_) -> false.
   

%% channelid  = 5( %x41-5A / digit )   ; 5( A-Z / 0-9 )

channelid(CID) when length(CID) == 5 -> channelid2(CID);
channelid(_) -> false.

channelid2([C|R]) when C >= 16#41, C =< 16#5A -> channelid2(R);
channelid2([C|R]) ->
    case digit([C]) of 
        true -> channelid2(R);
        false -> false
    end;
channelid2([]) -> true;
channelid2(_) -> false.


%% ------------------------------------------------------------------ %%

%% user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
%%                  ; any octet except NUL, CR, LF, " " and "@"

user(S) when length(S) >= 1 -> user2(S);
user(_) -> false.

user2([C|R]) when C >= 16#01, C =< 16#09 -> user2(R);
user2([C|R]) when C >= 16#0B, C =< 16#0C -> user2(R);
user2([C|R]) when C >= 16#0E, C =< 16#1F -> user2(R);
user2([C|R]) when C >= 16#21, C =< 16#3F -> user2(R);
user2([C|R]) when C >= 16#41, C =< 16#FF -> user2(R);
user2([]) -> true;
user2(_) -> false.


%% key        =  1*23( %x01-05 / %x07-08 / %x0C / %x0E-1F / %x21-7F )
%%                  ; any 7-bit US_ASCII character,
%%                  ; except NUL, CR, LF, FF, h/v TABs, and " "

key(S) -> key(S, length(S)).

key(S, L) when L >= 1, L =< 23 -> key2(S);
key(_, _) -> false.

key2([C|R]) when C >= 16#01, C =< 16#05 -> key2(R);
key2([C|R]) when C >= 16#07, C =< 16#08 -> key2(R);
key2([C|R]) when C == 16#0C -> key2(R);
key2([C|R]) when C >= 16#0E, C =< 16#1F -> key2(R);
key2([C|R]) when C >= 16#21, C =< 16#7F -> key2(R);
key2([]) -> true;
key2(_) -> false.
    

%% letter     =  %x41-5A / %x61-7A       ; A-Z / a-z

letter([C|R]) when C >= 16#41, C =< 16#5A -> letter(R);
letter([C|R]) when C >= 16#61, C =< 16#7A -> letter(R);
letter([]) -> true;
letter(_) -> false.
     

%% digit      =  %x30-39                 ; 0-9

digit([C|R]) when C >= 16#30, C =< 16#39 -> digit(R);
digit([]) -> true;
digit(_) -> false.


%% hexdigit   =  digit / "A" / "B" / "C" / "D" / "E" / "F"

hexdigit([$A|R]) -> hexdigit(R);
hexdigit([$B|R]) -> hexdigit(R);
hexdigit([$C|R]) -> hexdigit(R);
hexdigit([$D|R]) -> hexdigit(R);
hexdigit([$E|R]) -> hexdigit(R);
hexdigit([$F|R]) -> hexdigit(R);
hexdigit([Other|R]) ->
    case digit([Other]) of
        true -> hexdigit(R);
        false -> false
    end;
hexdigit([]) -> true;
hexdigit(_) -> false.


%% special    =  %x5B-60 / %x7B-7D
%%                 ; "[", "]", "\", "`", "_", "^", "{", "|", "}"

special([C|R]) when C >= 16#5B, C =< 16#60 -> special(R);
special([C|R]) when C >= 16#7B, C =< 16#7D -> special(R);
special([]) -> true;
special(_) -> false.


%% ------------------------------------------------------------------ %%

%% Okay this is silly, it allows "" but not "*" !
%% mask       =  *( nowild / noesc wildone / noesc wildmany )

mask(S) -> octet(S).

octet([C|R]) when C >= 16#01 -> octet(R);
octet([]) -> true;
octet(_) -> false.

%% wildone([16#3F]) -> true;
%% wildone([]) -> true;
%% wildone(_) -> false.

%% wildmany([16#2A]) -> true;
%% wildmany([]) -> true;
%% wildmany(_) -> false.

%% nowild([C|R]) when C >= 16#01, C =< 16#29 -> nowild(R);
%% nowild([C|R]) when C >= 16#2B, C =< 16#3E -> nowild(R);
%% nowild([C|R]) when C >= 16#40, C =< 16#FF -> nowild(R);
%% nowild([]) -> true;
%% nowild(_) -> false.

%% noesc([C|R]) when C >= 16#01, C =< 16#5B -> noesc(R);
%% noesc([C|R]) when C >= 16#5D, C =< 16#FF -> noesc(R);
%% noesc([]) -> true;
%% noesc(_) -> false.

     
%% ------------------------------------------------------------------ %%

%% Utility

tryfuncs(C, Fs) when is_integer(C) ->
    lists:any(fun(F) -> F([C]) end, Fs);
tryfuncs(S, Fs) ->
    lists:all(fun(C) -> tryfuncs(C, Fs) end, S).


test() ->

    io:format("Message: ~p~n", [message(
                                  ":Brend!n=brendonh@220-131-227-200.HINET-IP.hinet.net PRIVMSG veracity_bot :Also, hi")]).


