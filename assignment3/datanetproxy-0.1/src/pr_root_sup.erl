-module(pr_root_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(LSock) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

init([LSock]) ->
    PrSup = {pr_sup, {pr_sup, start_link, [LSock]}, permanent,
	     3000, supervisor, [pr_server]},
%    IbrowseSup = {ibrowse, {ibrowse_sup, start_link, []},
%		  permanent, 3000, supervisor, [ibrowse]},
    PrTracker = {pr_tracker, {pr_tracker, start_link, []},
		 permanent, 3000, worker, [pr_tracker]},
    PrParser = {pr_nodeparser, {pr_nodeparser, start_link, []},
		permanent, 3000, worker, [pr_nodeparser]},
    Children = [PrSup, PrParser, PrTracker],
    RestartStrategy = {one_for_one, 10, 1},
    {ok, {RestartStrategy, Children}}.
	
	
