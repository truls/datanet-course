-module(pr_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("rsakey.hrl").
-include("log.hrl").

start(_Type, _StartArgs) ->
    % Add the properly encoded RSA keys to the environment
    % in mpint format as expected by the crypto: functions
    ok = application:set_env(proxy, 'RSAN', crypto:mpint(?RSAN)),
    ok = application:set_env(proxy, 'RSAE', crypto:mpint(?RSAE)),
    ok = application:set_env(proxy, 'RSAD', crypto:mpint(?RSAD)),
    % Open listening socket
    ?DEBUG("Got here", []),
    {ok, LSock} = gen_tcp:listen(8000, [binary, {active, true}, {packet, http_bin}]),
%    io:format("Listening to 
    %case pr_sup:start_link(LSock) of
    case pr_root_sup:start_link(LSock) of
	{ok, Pid} ->
	    ?DEBUG("Got here", []),
	    pr_sup:start_child(),
	    {ok, Pid};
	Other ->
	    ?DEBUG("Supervisor startup failed ~p", [Other]),
	    {error, Other}
    end.

stop(_State) ->
    ok.


