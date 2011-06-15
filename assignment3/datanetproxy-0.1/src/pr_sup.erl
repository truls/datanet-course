-module(pr_sup).

-behaviour(supervisor).

-export([start_link/1, start_child/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(LSock) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

start_child() ->
    % Has format SupRef, ChildId. We can leave ChildId empty as
    % we use the simple_one_for_one strategy
    supervisor:start_child(?SERVER, []).

init([LSock]) ->
    Server = {pr_server, {pr_server, start_link, [LSock]},
	      temporary, brutal_kill, worker, [pr_server]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

