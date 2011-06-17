%% A gen_server for keeping track of the nodes in and
%% and maintaing registration with the erlang datanet tracker
%% located at datanet2011tracker.appspot.com


-module(pr_tracker).

-behaviour(gen_server).

-export([start_link/0, stop/1, get_peer/1, is_allowed/1, get_lastsync/0, blacklist_peer/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("log.hrl").
-include("peerlist.hrl").

-record(state, {peers = #peerlist{} , lastsync = 0, blacklist = [], keys, timer, verification = false}).

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
    % We only fetch the trackers publickey upon initalization
    {ok, PubkeyURL} = application:get_env(proxy, pubkey),
    case ibrowse:send_req(PubkeyURL, [], get) of
	{ok, "200", _Headers, Body} ->
	    ?INFO("Fetched pubkey"),
	    State = #state{keys = pr_nodeparser:parse_key(Body)},
	    ?DEBUG("Done calling nodeparser", []),
	    TimerRef = erlang:start_timer(0, self(), tracker_timer),
	    {ok, State#state{timer = TimerRef}};
	_ ->
	    ?ERR("Fetching of public keys failed"),
	    {error, #state{}}
    end.

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
    case refresh_and_register(State) of
	{ok, Newstate} ->
	    Peerlist = Newstate#state.peers,
	    TimerRef = erlang:start_timer(Peerlist#peerlist.minwait * 1000 + 100, self(), tracker_timer),
						%_TimerRef = erlang:start_timer(120000, self(), tracker_timer),
	    {noreply, Newstate#state{timer = TimerRef}};
	{error, Newstate} ->
	    {stop, error, Newstate}
    end.

code_change(_, _, _) ->
    noreply.

terminate(_, _) ->
    noreply.
	

% Internal API

append_padding(String) ->
   <<0:8/integer-big, -1:760/integer-big, String/binary>>.

to_mpint(X) ->
    <<(byte_size(X)):32/integer-big, X/binary>>.

content_signature(Content, #state{peers = Peerlist} = _State) ->
    SHA256 = erlsha2:sha256(Content ++ Peerlist#peerlist.nonce),
    PSHA256 = append_padding(SHA256),
    Mp_data = to_mpint(PSHA256),
    ISig = crypto:mod_exp(Mp_data,
			  nook(application:get_env(proxy, 'RSAD')),
			  nook(application:get_env(proxy, 'RSAN'))),
    <<_:32/integer-big, Signature/binary>> = ISig,
    base64:encode(Signature).

verify_content(Signature, Content, #key{n = N, e = E} = _Keys) ->
    PC = append_padding(erlsha2:sha256(Content)),
    PCi = to_mpint(PC),
    Si = to_mpint(base64:decode(Signature)),
    Vi = crypto:mod_exp(Si, E, N),
    PCi == Vi.		

refresh_and_register(#state{blacklist = Blacklist, peers = Peerlist, keys = Keys} = State) ->
    io:format("refreshing.... "),

    ReqBody = "port=7436&action=register&pub_key=" ++
	integer_to_list(crypto:erlint(nook(application:get_env(proxy, 'RSAN')))) ++ "+" ++
	integer_to_list(crypto:erlint(nook(application:get_env(proxy, 'RSAE')))),
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"},
	       {"Content-Signature", binary_to_list(content_signature(ReqBody, State))}],
    ?DEBUG("Sent body ~p~n With header ~p~n", [ReqBody, Headers]),

% Registration temporary disabled due to my getting locked out
% because of too frequent registrations. Now I just need to get 
% the list of peers
%case ibrowse:send_req("http://datanet2011tracker.appspot.com/peers.txt",
%		     [], post, "ip=213.239.205.47&port=8000&action=register") of
 %   case ibrowse:send_req(application:get_env(peerlist), [], get) of
    case ibrowse:send_req(nook(application:get_env(peerlist)), Headers, post, ReqBody) of
	{ok, "200", RHeaders, Body} ->
	    io:format("success~n"),
	    
	    ?DEBUGP("Begin response verification"),
	    Verified = verify_content(
			 proplists:get_value("Content-Signature", RHeaders),
			 Body,
			 Keys),
	    ?DEBUG("Verification status ~p", [Verified]),

	    case Verified of
		true ->
		    NewState = State#state{peers = pr_nodeparser:parse(Body, Blacklist),
					   lastsync = calendar:datetime_to_gregorian_seconds(
							calendar:local_time())},
		    Peerlist1 = NewState#state.peers,
		    case Peerlist1#peerlist.verified of
			true ->
			    ?DEBUGP("We got verified. Returning"),
			    {ok, NewState};
			false ->
			    ?DEBUGP("Didn't get returned. Trying again"),
			    % Maybe a badly named state variable. The verification variable
			    % will be set to true before the second verifcatin attempt (with
			    % the nonce include. If verification fails while verification is
			    % set to true the process will abort.
			    case NewState#state.verification of
				false ->
				    refresh_and_register(NewState#state{verification = true});
			       true ->
				    {error, NewState#state{verification = true}}
			    end
		    end;
		false ->
		    {error, State}
	    end;	
	Fail ->
	    io:format("Failed ~p~n", [Fail]),
	    {error, State}
    end.

blacklist_peer(BadPeer, #state{peers = Peerlist, blacklist = Blacklist} = State) ->
    {BLPeerlist, NewBlacklist} = blacklist_peer(Peerlist#peerlist.peers, [], BadPeer, Blacklist),
    NewPeerlist = Peerlist#peerlist{peers = BLPeerlist},
    State#state{peers = NewPeerlist, blacklist = NewBlacklist}.
blacklist_peer([], Newpeerlist, _BadPeer, Blacklist) ->
    {Newpeerlist, Blacklist};
blacklist_peer([Peer|Peerlist], Newpeerlist, BadPeer, Blacklist) ->
    
    case {Peer#peer.ip, Peer#peer.port} of
	{I, P} when I == BadPeer#peer.ip andalso
		    P == BadPeer#peer.port ->
	    blacklist_peer(Peerlist, Newpeerlist, BadPeer, [BadPeer|Blacklist]);
	_ ->
	    blacklist_peer(Peerlist, [Peer|Newpeerlist], BadPeer, Blacklist)
    end.

nook({ok, K}) ->
    K.
	
%in_list([X|Xs], K) ->
%    case X of
%	K -> true;
%	_ -> in_list(Xs, K)
%    end;
%in_list([], K) ->
%    false. 

