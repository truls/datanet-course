-module(pr_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    % Open listening socket
    {ok, LSock} = gen_tcp:listen(8000, [binary, {active, true}, {packet, http_bin}]),
%    io:format("Listening to 
    %case pr_sup:start_link(LSock) of
    case pr_root_sup:start_link(LSock) of
	{ok, Pid} ->
	    pr_sup:start_child(),
	    {ok, Pid};
	Other ->
	    {error, Other}
    end.

stop(_State) ->
    ok.


