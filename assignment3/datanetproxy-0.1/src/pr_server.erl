-module(pr_server).

-behaviour(gen_server).

-export([start_link/1, stop/1]).

% gen_server stuff

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {lsock, socket, request, content_remaining, headers = [],
	        req_method, req_host, req_proto, url, max_forward = -1,
		req_id, req_body = [], req_has_body = false, ibrowse_rid,
	        has_via = false, via_super = false, via = []}).

-include("peerlist.hrl").

start_link(LSock) ->
    % Leave name {local, name} so that the process
    % remains unregistered. This enables us to start
    % mutliple processes using the pr_sup:start_child() call
    gen_server:start_link(?MODULE, [LSock], []).

stop(_State) ->
    gen_server:cast(?SERVER, stop).


% Callbacks
init([LSock]) ->
    % Kind of a hack, return a timeout of 0 so that handle_info(
    {ok, #state{lsock = LSock}, 0}.

handle_call(_, _, State) ->
    {reply, 0, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.


handle_info({ibrowse_async_headers, Req_id, Response, Headers}, State) ->
    % Return initial request line
    io:format("Got reply: ~p ~p ~n", [Response, sanitize_headers(Headers)]),
    send_response(http_response(list_to_integer(Response),
				sanitize_headers([{'Via', State#state.via}|Headers]), []),
		  State),
    {noreply, State};
handle_info({ibrowse_async_response, Req_Id, {error, req_timedout}}, State) ->
    io:format("Request ~p timed out~n", [Req_Id]),
    % Requests which haven't been responded to within inactivity_timeout ends
    % up here. Figure out how to handle.
    http_error(504, State), %Send "Gateway timeout"
    {stop, normal, State};
handle_info({ibrowse_async_response, Req_id, Data}, State) ->
    %io:format("Data was: ~p~n", [Data]),
    %ok = gen_tcp:send(State#state.socket, Data),
    send_response(Data, State),
    io:format("Length of chunk~p~n", [string:len(Data)]),
%    io:format("Send chunck\n"),
    ok = ibrowse:stream_next(Req_id),
    io:format("Sent chunck\n"),
    {noreply, State};
handle_info({ibrowse_async_response_end, _Req_id}, State) ->
    gen_tcp:close(State#state.socket),
    io:format("Closed socket~n"),
    {stop, normal, State};
handle_info({http, _Socket, {http_error, String}}, State) ->
    io:format("Got error ~p~n", [String]),
    {stop, normal, State};
handle_info({http, _Socket, {http_request, Method, {absoluteURI, Proto, Host, Port, Path}, _} = Request}, State) ->
    case pr_tracker:is_allowed(to_list(Host)) of
	false ->
	    http_error(403, State),
	    {stop, normal, State};
	true ->
	    inet:setopts(State#state.socket, [{active, once}]),
	    URL = assemble_url(Proto, Host, Port, Path),
	    case method(Method) of
		unsupported ->
		    http_error(501, State),
		    {stop, normal, State};
		MethodAtom ->
		    {noreply, State#state{request = Request,
					  req_method = MethodAtom,
					  req_host = Host, url = URL}}
	    end
	end;
handle_info({http, _Socket, {http_header, _, Field, _, Value}}, State) ->
    inet:setopts(State#state.socket, [{active, once}]),
%    io:format("Process header ~p ~p~n", [Field, Value]),
    {noreply, header(to_atom(Field), Value, State)};
handle_info({http, _Socket, http_eoh}, #state{has_via = HasVia, max_forward = MaxForward} = State) ->
    inet:setopts(State#state.socket, [{active, true}, {packet, raw}]),
%    io:format("We are done! got request ~p, and headers ~p~n", [State#state.request, State#state.headers]),
    case HasVia of
	false ->
	    NewState = header('Via', "", State);
	true ->
	    NewState = State
    end,
    io:format("Max-Forward was ~p~n", [MaxForward]),
    if MaxForward < 0 ->
	    % Request contained no Max-forward header. Add it and set it to 3 hops
	    NewState_1 = header('Max-Forward', "3", NewState);
      true ->
	    NewState_1 = NewState
    end,
						%    NewState_2 = header('Via-Super', "", NewState_1),
    NewState_2 = NewState_1,
    ReversedState = NewState_2#state{headers = sanitize_headers(lists:reverse(NewState_2#state.headers))},
    case ReversedState#state.req_has_body of
	true ->
	    {noreply, ReversedState};
	false ->
	    {noreply, handle_request(ReversedState)}
%	    {stop, normal, handle_request(State)}
    end;
handle_info({tcp, Socket, Data}, State) when is_binary(Data) ->
%    io:format("Received part~p~n", [Data]),
%    io:format("Length of this segment was: ~p~n", [erlang:byte_size(Data)]),
    
    %FIXME: This function is incapable of efficiently handling large bodies
    %       We'll need to find a way of dispatching a response before 
    %       we actually start receiving data.

    %gen_tcp:send(Socket, "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\nfoo"),
    %gen_tcp:close(Socket),
    %{stop, normal, handle_request(State#state{req_body = Data})};
    {noreply, handle_request(State#state{req_body = Data})};
handle_info({tcp_closed, _Socket}, State) ->
    io:format("tcp_closed received~n"),
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    pr_sup:start_child(),
    % Use active: once to enable flow-control
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{socket = Socket}}.


code_change(_, _, _) ->
    noreply.


terminate(_, _) ->
    noreply.


% Internal API

header('Content-Length' = Name, Value, State) ->
    ContentLength = list_to_integer(binary_to_list(Value)),
    State#state{content_remaining = ContentLength,
		headers = [{Name, to_list(Value)}|State#state.headers],
	        req_has_body = true};
header('Max-Forward' = Name, Value, State) ->
    io:format("Set Max-Forward header~n"),
    Forwards = list_to_integer(to_list(Value)),
    if Forwards > 5 ->
	    State#state{max_forward = 5,
			headers = [{Name, "5"}|State#state.headers]};
       true ->
	    Ret = State#state{max_forward = Forwards,
			      headers = [{Name, integer_to_list(Forwards - 1)}|State#state.headers]},
	    io:format("~p~n", [Ret]),
	    Ret
    end;
header('Via' = Name, Value, State) ->
%    State;
    NewVia = to_list(Value) ++ " HTTP/1.1 213.239.205.47:8000 (datanetproxy,truls@diku.dk)",
    State#state{headers = [{Name, NewVia}|
			   State#state.headers],
	        has_via = true, via = NewVia};
header('Via-Super' = Name, Value, State) ->
    State#state{via_super = true,
		headers = [{Name, to_list(Value)}|State#state.headers]};
header(Name, Value, State) ->
    State#state{headers = [{to_atom(Name), to_list(Value)}|State#state.headers]}.


% Filter out hop-by-hop headers
%      - Connection
%      - Keep-Alive
%      - Proxy-Authenticate
%      - Proxy-Authorization
%      - TE
%      - Trailers
%      - Transfer-Encoding
%      - Upgrade

sanitize_headers(Headers) ->
	[{'Connection', "close"}] ++ %[{'Via-Super', "foo"}] ++
	lists:filter(fun({Header, _Value}) ->
			    case Header of
				"Connection" -> false;
				"Keep-Alive" -> false;
				"Proxy-Authenticate" -> false;
				"Proxy-Authorization" -> false;
				"TE" -> false;
				"Transfer-Encoding" -> false;
				"Upgrade" -> false;
				"Proxy-Connection" -> false;
				"Accept-Encoding" -> false;
				_ -> true
			    end end, Headers).


%prep_response_headers(Headers) ->
%    prep_response_headers(Headers, []).
%prep_response_headers([{'Via', Value}|Headers]

% Receive the next chunk of code from the code
% recv_block(State)

% Handle proxy-to-proxy request
handle_request(#state{via_super = ViaSuper} = State) when State#state.max_forward > 0 ->
    NextHop = pr_tracker:get_peer(ViaSuper),
    io:format("Sending request via ~p~n", [NextHop]),
    io:format("ibrowse:send_req(~p, ~p, ~p, ~p, ~p, ~p)~n", [State#state.url, State#state.headers, State#state.req_method, State#state.req_body, [], infinity]),
    case ibrowse:send_req(State#state.url, State#state.headers,
			  State#state.req_method, State#state.req_body,
			  [{stream_to, {self(), once}},
			   {preserve_chunked_encoding, true},
			   {headers_as_is, true},
			   {connect_timeout, 10000},
			   {inactivity_timeout, 30000},
			   {proxy_host, NextHop#peer.ip},
			   {proxy_port, NextHop#peer.port}],
			  infinity) of
	{ibrowse_req_id, ReqID} ->
	    State#state{ibrowse_rid = ReqID};
%	{conn_failed, {error, Reason}} ->
%	{error, {conn_failed, {_, _}}} ->
	    % Bad proxy... Blacklist!
	{error, Reason} ->
	    io:format("Blacklisted peer because of ~p~n", [Reason]),
	    pr_tracker:blacklist_peer(NextHop),
	    handle_request(State)
%	{error, Reason} ->
%	    io:format("Send generic error message ~p~n", [Reason])
    end;
handle_request(State) when State#state.max_forward =< 0 ->
    io:format("ibrowse:send_req(~p, ~p, ~p, ~p, ~p, ~p)~n", [State#state.url, State#state.headers, State#state.req_method, State#state.req_body, [], infinity]),
        case ibrowse:send_req(State#state.url, State#state.headers,
			  State#state.req_method, State#state.req_body,
			  [{stream_to, {self(), once}},
			   {preserve_chunked_encoding, true},
			   {headers_as_is, true},
			   {connect_timeout, 50000},
			   {inactivity_timeout, 20000}],
			      infinity) of
	    {ibrowse_req_id, ReqID} ->
		State#state{ibrowse_rid = ReqID};
	    {error, Reason} ->
		io:format("Send generic error message ~p ~n", [Reason])
	end.

    % Request a remote ting
%    io:format("Handle, request ~p   ~p~n", [State#state.url, State#state.headers]),
%    Options = [{stream_to, {self(), once}}, {preserve_chunked_encoding, true}],

    

method('GET') -> get;
method('POST') -> post;
method('HEAD') -> head;
method(_) -> unsupported.

header_format([{Key, Value}|Headers]) ->
    to_list(Key) ++ ": " ++ to_list(Value) ++ "\r\n" ++ header_format(Headers);
header_format([]) -> "".


http_error(Code, State) ->
    ErrLine = erlang:integer_to_list(Code)  ++ " " ++ httpd_util:reason_phrase(Code),
    ErrBody = html_content(ErrLine, ["<h1>" ++ ErrLine ++ "</h1>"]),
    send_response(http_response(Code, [], ErrBody), State),
    gen_tcp:close(State#state.socket).

send_response(Response, State) ->
    ok = gen_tcp:send(State#state.socket, Response).

http_response(Code, Headers, []) ->
    "HTTP/1.1 " ++ 
	erlang:integer_to_list(Code)
	++ " " ++
	httpd_util:reason_phrase(Code)
	++ "\r\n" ++
	header_format(Headers) ++ "\r\n";
http_response(Code, Headers, Body) ->
    http_response(Code,
		  Headers, []) ++ Body.

html_content(Title, Body) ->
    string:join(["<!DOCTYPE html>"
		 "<html>",
		 "<head>",
		 "<title>" ++ Title ++ "</title>",
		 "<meta encoding=\"utf-8\">",
		 "</head>",
		 "<body>"]
		++ Body ++
		    ["<hr />",
		     "<p>Datanet proxy, operated by truls@diku.dk</p>",
		     "</body>",
		     "</html>"], "\n").

assemble_url(Proto, Host, Port, Path) -> 
    atom_to_list(Proto) ++ "://" ++ binary_to_list(Host) ++
	case Port of
	    undefined ->
		"";
	    _ ->
		":" ++ integer_to_list(Port)
	end  ++ binary_to_list(Path).

to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_list(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_list(Value) ->
    Value.

to_atom(V) when is_binary(V) ->
    list_to_atom(binary_to_list(V));
to_atom(V) when is_list(V) ->
    list_to_atom(V);
to_atom(V) ->
    V.



