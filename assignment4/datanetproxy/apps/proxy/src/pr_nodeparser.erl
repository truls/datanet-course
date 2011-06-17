-module(pr_nodeparser).

-behaviour(gen_server).

-export([start_link/0, stop/1, parse_list/3, parse/2, parse_key/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {from, body, blacklist = []}).

-include("log.hrl").
-include("peerlist.hrl").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop(_State) ->
    gen_server:cast(?SERVER, stop).

parse(Body, Blacklist) ->
    gen_server:call(?SERVER, {parse, Body, Blacklist}).

parse_key(Body) ->
    gen_server:call(?SERVER, {parse_key, Body}).

init([]) ->
    {ok, #state{}}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({parse, Body, Blacklist}, From, _State) ->
    {noreply, #state{from = From, body = Body, blacklist = Blacklist}, 0};
handle_call({parse_key, Body}, _From, State) ->
    {reply, do_parse_key(Body), State}.

handle_info(timeout, #state{from = From, body = Body} = State) ->
    _Result = gen_server:reply(From, parse_list(Body, State)),
    {noreply, State}.

code_change(_, _, _) ->
    noreply.

terminate(_, _) ->
    noreply.

do_parse_key(Body) ->
    Splitted = [string:tokens(X, ": ") || X <- string:tokens(Body, "\r\n")],
    do_parse_key(Splitted, #key{}).

do_parse_key([["N", N]|Body], Result) ->
    do_parse_key(Body, Result#key{n = crypto:mpint(list_to_integer(N))});
do_parse_key([["E", N]|Body], Result) ->
    do_parse_key(Body, Result#key{e = crypto:mpint(list_to_integer(N))});
do_parse_key([], Result) ->
    Result.

parse_list(Body, State) ->
%    Splitted = lists:map(fun(Line) -> string:tokens(Line, " \t") end,
%			 string:tokens(Body, "\r\n")),
    Splitted = [string:tokens(X, " \t") || X <- string:tokens(Body, "\r\n")],
    %io:format("~p~n", [Splitted]),
    parse_list(Splitted, #peerlist{}, State).

parse_list([], Result, _State) ->
    %{State, Result};
    Result;
% XXX: These will fail if there is no space between the field and the value
%      as allowed by the format specification
parse_list([["Verified:", Value]|Rest], Result, State) ->
    ?INFO("Tracker returned verification status: " ++ Value),
    parse_list(Rest, Result#peerlist{verified = boolstring_to_atom(Value)}, State);
parse_list([["Nonce:", Value]|Rest], Result, State) ->
    ?DEBUG("Got nonce ~p", [Value]),
    parse_list(Rest, Result#peerlist{nonce = Value}, State);
parse_list([["Expire:", Value]|Rest], Result, State) ->
%    io:format("1"),
    parse_list(Rest, Result#peerlist{expire = list_to_integer(Value)}, State);
parse_list([["Min-Wait:", Value]|Rest], Result, State) ->
%    io:format("2"),
    parse_list(Rest, Result#peerlist{minwait = list_to_integer(Value)}, State);
parse_list([[Domain]|Rest], #peerlist{whitelist = Whitelist} = Result, State) ->
%    io:format("3"),
    parse_list(Rest, Result#peerlist{whitelist = [Domain|Whitelist]}, State);
    
parse_list([[Ip, Port, _Day, Date, Month, Year, Time, _Zone, Superpeer, N, E]|Rest],
	   #peerlist{peers = Peers} = Result, State) ->
%    io:format("4"),
    [Hour, Minute, Second] = [list_to_integer(X) || X <- string:tokens(Time, ":")],
    NewPeer = #peer{ip = Ip,
		    port = list_to_integer(Port),
		    datetime = calendar:datetime_to_gregorian_seconds({{list_to_integer(Year), monthn(Month), list_to_integer(Date)},
								   {Hour, Minute, Second}}),
		    superpeer = Superpeer =:= "SUPERPEER",
		    n = case N of
			    undef -> 0;
			    M -> crypto:mpint(list_to_integer(M))
			end,
		    e = case E of 
			    undef -> 0;
			    F -> crypto:mpint(list_to_integer(F))
			end
		   },
    %case in_blacklist(State#state.blacklist, NewPeer) of
%	false ->
%	    parse_list(Rest, Result, State);
%	true ->
	    parse_list(Rest, Result#peerlist{peers = [NewPeer|Peers]}, State);
 %   end;
parse_list([[Ip, Port, Day, Date, Month, Year, Time, Zone, N, E]|Rest], Result, State) ->
    parse_list([[Ip, Port, Day, Date, Month, Year, Time, Zone, false, N, E]|Rest], Result, State);
parse_list([[Ip, Port, Day, Date, Month, Year, Time, Zone, Superpeer]|Rest], Result, State) ->
    parse_list([[Ip, Port, Day, Date, Month, Year, Time, Zone, Superpeer, undef, undef]|Rest],
	       Result, State);
parse_list([[Ip, Port, Day, Date, Month, Year, Time, Zone]|Rest], Result, State) ->
    parse_list([[Ip, Port, Day, Date, Month, Year, Time, Zone, false, undef, undef]|Rest], Result, State).

in_blacklist([Peer|Blacklist], NewPeer) ->
    case {Peer#peer.ip, Peer#peer.port} of
	{I, P} when I == NewPeer#peer.ip andalso P == NewPeer#peer.port andalso
	      not NewPeer#peer.datetime > Peer#peer.datetime ->
	    true;
%	{NewPeer#peer.ip, NewPeer#peer.port} ->
	    
	{_, _} ->
	    in_blacklist(Blacklist, NewPeer)
    end;
in_blacklist([], _NewPeer) ->
    false.

boolstring_to_atom("False") -> 
    false;
boolstring_to_atom("True") -> 
    true;
boolstring_to_atom(_) -> 
    false.

monthn("Jan") -> 01;
monthn("Feb") -> 02;
monthn("Mar") -> 03;
monthn("Apr") -> 04;
monthn("May") -> 05;
monthn("Jun") -> 06;
monthn("Jul") -> 07;
monthn("Aug") -> 08;
monthn("Sep") -> 09;
monthn("Oct") -> 10;
monthn("Nov") -> 11;
monthn("Dec") -> 12. 
	      
