%% A gen_server for keeping track of the nodes in and
%% and maintaing registration with the erlang datanet tracker
%% located at datanet2011tracker.appspot.com


-module(pr_tracker).

-behaviour(gen_server).

-export([start_link/0, stop/1, get_peer/1, is_allowed/1, get_lastsync/0, blacklist_peer/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {peers, lastsync = 0, blacklist = []}).

-include("peerlist.hrl").

% Public API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop(_State) ->
    gen_server:cast(?SERVER, stop).

get_peer(SuperPeer) ->
    gen_server:call(?SERVER, {get_peer, SuperPeer}).

is_allowed(Domain) ->
    gen_server:call(?SERVER, {is_allowed, Domain}).

get_lastsync() ->
    gen_server:call(?SERVER, lastsync).

blacklist_peer(Peer) ->
    gen_server:cast(?SERVER, {blacklist, Peer}).

% Callbacks
init([]) ->
    _TimerRef = erlang:start_timer(0, self(), tracker_timer),
    {ok, #state{}}.

handle_call({get_peer, SuperPeer}, _From, #state{peers = Peerlist} = State) ->
    % FIXME: This is slow and unnecessary.
    case SuperPeer of
	true ->
	    Peers = [X || X <- Peerlist#peerlist.peers, X#peer.superpeer];
%lists:filter(fun(Peer) -> Peer#peer.superpeer end,
%				 Peerlist#peerlist.peers);
	false ->
	    Peers = Peerlist#peerlist.peers
    end,

    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    {reply, lists:nth(random:uniform(length(Peers)), Peers), State};
handle_call({is_allowed, Domain}, _From, #state{peers = Peerlist} = State) ->
    % FIXME: This will accept requests for evilgoogle.com when google.com is whitelisted
    {reply, length([X || X <- Peerlist#peerlist.whitelist, string:rstr(Domain ++ ".", X) > 0]) > 0, State};
handle_call(lastsync, _From, State) ->
    {reply, State#state.lastsync, State}.

handle_cast({blacklist, Peer}, State) ->
    {noreply, State}.

% FIXME: This is bad as it creates a syncronous call which blocks
%        the server (and therefore the entire proxy) until
%        the list has been refreshed.
handle_info({timeout, _, tracker_timer}, State) ->
    NewState = refresh_and_register(State),
    Peerlist = NewState#state.peers,
    _TimerRef = erlang:start_timer(Peerlist#peerlist.minwait * 1000 + 100, self(), tracker_timer),
    {noreply, NewState}.

code_change(_, _, _) ->
    noreply.

terminate(_, _) ->
    noreply.
	

% Internal API

refresh_and_register(#state{blacklist = Blacklist} = State) ->
    io:format("refreshing.... "),

% Registration temporary disabled due to my getting locked out
% because of too frequent registrations. Now I just need to get 
% the list of peers
%    case ibrowse:send_req("http://datanet2011tracker.appspot.com/peers.txt",
%		     [], post, "ip=213.239.205.47&port=8000&action=register") of
    case ibrowse:send_req("http://datanet2011tracker.appspot.com/peers.txt", [], get) of
	{ok, "200", _Headers, Body} ->
	    io:format("success~n"),
	    State#state{peers = pr_nodeparser:parse(Body, Blacklist),
			lastsync = calendar:datetime_to_gregorian_seconds(
				     calendar:local_time())};
	Fail ->
	    io:format("Failed ~p~n", [Fail]),
	    State
    end.


blacklist_peer(BadPeer, #state{peers = Peerlist, blacklist = Blacklist} = State) ->
    {BLPeerlist, NewBlacklist} = blacklist_peer(Peerlist#peerlist.peers, [], BadPeer, Blacklist),
    NewPeerlist = Peerlist#peerlist{peers = BLPeerlist},
    State#state{peers = NewPeerlist, blacklist = NewBlacklist}.
blacklist_peer([], Newpeerlist, BadPeer, Blacklist) ->
    {Newpeerlist, Blacklist};
blacklist_peer([Peer|Peerlist], Newpeerlist, BadPeer, Blacklist) ->
    
    case {Peer#peer.ip, Peer#peer.port} of
	{I, P} when I == BadPeer#peer.ip andalso
		    P == BadPeer#peer.port ->
	    blacklist_peer(Peerlist, Newpeerlist, BadPeer, [BadPeer|Blacklist]);
	_ ->
	    blacklist_peer(Peerlist, [Peer|Newpeerlist], BadPeer, Blacklist)
    end.
	
%in_list([X|Xs], K) ->
%    case X of
%	K -> true;
%	_ -> in_list(Xs, K)
%    end;
%in_list([], K) ->
%    false. 

