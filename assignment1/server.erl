%% Datanet assignment 1
%% Simple HTTP Server.
%% For full assignment text. See file datanet-assignment1.pdf
%% Run with
%%
%% Truls Asheim <truls@diku.dk> 2011

-module(server).
-export([start/3, start/0]).

-define(MAXCONN, 10).

start() ->
    start("localhost", 8000, "/home/truls").
start(Hostname, Port, Path) ->
    {ok, Ip} = inet:getaddr(Hostname, inet),
    {ok, Listener} = gen_tcp:listen(Port, [{ip, Ip}]),
    io:format("Listening to ~p:~p and serving ~p~n", [Hostname, Port, Path]),
    spawn(fun() -> lstn_loop(Listener) end).

lstn_loop(Listen)  ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> lstn_loop(Listen) end),
    rcv_loop(Socket),
    gen_tcp:close(Socket).

rcv_loop(Socket) ->
    receive
	{tcp, Socket, Bin} ->
	    io:format("We received~p~n", [Bin]),
	    Tokens = string:tokens(Bin, "\r\n"),
	    io:format("In list form this is ~p~n\n", [Tokens]),
	    TokenizedTokens = lists:map(fun(T) -> string:tokens(T, " ") end, Tokens),
	    io:format("This gives ~p~n\n", [TokenizedTokens]),

	    case TokenizedTokens of 
		[["GET", Path, _]|_] ->
		    io:format("The request type is: GET and the path is ~p~n", [Path]);
		[["HEAD", Path, _]|_] ->
		    io:format("The request type is: HEAD and the oath is ~p~n", [Path]);
		[[Request,_,_]|_] ->
		    io:format("Unsupported request: ~p~n", [Request])
	    end,

	    gen_tcp:send(Socket, "bar\n")
    end.

