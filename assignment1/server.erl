-module(server).
-export([start_server/3, start_server/0]).
%-import([tcp_gen]).
%%-export([main/1]).

start_server() ->
    start_server("localhost", 8000, "/home/truls").
start_server(Hostname, Port, Path) ->
    {ok, Ip} = inet:getaddr(Hostname, inet),
    {ok, Listener} = gen_tcp:listen(Port, [{ip, Ip}]),
    lstn_loop(Listener, 0).

lstn_loop(Listen, Counter) when Counter < 5  ->
    {ok, Socket} = gen_tcp:accept(Listen),
    rcv_loop(Socket),
    gen_tcp:close(Socket),
    lstn_loop(Listen, Counter + 1).
    

rcv_loop(Socket) ->
    io:format("foo bar got connection"),
    gen_tcp:send(Socket, "bar\n").

      


