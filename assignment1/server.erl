%% Datanet assignment 1
%% Simple HTTP Server implemented in erlang. Erlang R14A01 or newer
%% is required to run this code.
%% 
%% Run with
%% $ cd /path/to/server.erl #This file
%% $ erl
%% 1> c(server).
%% 2> server:start().
%%
%% Truls Asheim <truls@diku.dk> 2011

-module(server).
-export([start/3, start/0]).
%-compile(export_all).
-include_lib("kernel/include/file.hrl").

% Configuration is done here.
% Quick hack. Quite ugly and unerlangish. Could be a lot cooler.
% atom() -> term()
config(Key) ->
    Conf = [{port, 8000},
	    {hostname, "localhost"},
	    {document_root, "/home/truls"},
	    {document_index, ["index.html", "index.htm"]}],
    {_, Value} = lists:keyfind(Key, 1, Conf),
    Value.

% Possible bottleneck. All file requests uses this function for mime lookup
mime_lookup() ->
    MimeFile = "/etc/mime.types",
    {ok, MimeTypes} = httpd_conf:load_mime_types(MimeFile),
    mime_lookup(MimeTypes).
mime_lookup(MimeTypes) ->
    receive
	{From, Ext} ->
	    "." ++ Extension = Ext,
	    % Is octet-stream a nice default value?
	    From ! proplists:get_value(Extension, MimeTypes, "application/octet-stream")
    end,
    mime_lookup(MimeTypes).

start() ->
    start(config(hostname), config(port), config(document_root)).
start(Hostname, Port, Path) ->
    {ok, Ip} = inet:getaddr(Hostname, inet),
    {ok, Listener} = gen_tcp:listen(Port, [{ip, Ip}, binary]),
    io:format("Listening to ~p:~p and serving ~p~n", [Hostname, Port, Path]),
    MimePid = spawn(fun() -> mime_lookup() end),
    register(mime_handler, MimePid),
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
%	    case erlang:decode_packet(http, Bin, []) of

%		{ok, {http_request, Method, {abs_path, URI}, {1,1}}, Rest} -> 
	    case http_request_parse(Bin) of
		{ok, {Method, URI, {1,1}}, Rest, _} ->
		    case http_parse_headers(Rest) of
			error -> gen_tcp:send(Socket, http_error(400));
			Headers ->
			    Headers,
			    case Method of
				'GET' ->
				    io:format("GET ~p~n", [URI]),
				    gen_tcp:send(Socket, handle_path("/home/truls" ++ URI, false, Headers));
				'HEAD' ->
				    io:format("HEAD ~p~n", [URI]),
				    gen_tcp:send(Socket, handle_path("/home/truls" ++ URI, true, Headers));
				_ ->
				    io:format("Unsupported request: ~p~n", [Method]),
						% Send "Not implemented response code" as requred by the RFC
				    gen_tcp:send(Socket, http_error(501))
			    end
		    end;
			
						% Only deal with HTTP 1.1 clients
		{ok, {http_request, _, _, {_, _}},_} ->
		    gen_tcp:send(Socket, http_error(505));
		_ -> gen_tcp:send(Socket, http_error(400))
	    end;
	{tcp_closed, Socket} ->
	    io:format("Socket closed"),
	    ok		       
    end.

% binary() -> {ok, {Method = atom(), URI = list(), Version = {int(), int()}}, Headers = list(), BodyOffset = int()},
% binary() -> {error, Reason}
http_request_parse(Request) ->
    {LineBoundary,_} = binary:match(Request, <<"\r\n">>),
    RequestLine = erlang:binary_to_list(binary:part(Request, {0, LineBoundary})),
    {HeaderBoundary,_} = binary:match(Request, <<"\r\n\r\n">>),
    Headers = erlang:binary_to_list(binary:part(Request, {LineBoundary + 2, HeaderBoundary - (LineBoundary + 2)})),
    case string:tokens(RequestLine, " ") of
	[Method|[URI|[Version]]] ->
	    case http_parse_version(Version) of
		{error, Reason} -> Result = {error, Reason};
		VersionTuple -> 	
		    % TODO: URI might be urlencoded. Handle this.
		    Result = {ok, {erlang:list_to_atom(Method), URI, VersionTuple}, Headers, HeaderBoundary + 4}
	    end;
	 _ ->
	    Result = {error, "Request header does not contain three distinct tokens separated by sp"}
    end,
    Result.
% list() -> {Major = int(), Minor = int()}
http_parse_version("HTTP/" ++ Version) ->
    http_parse_version(string:tokens(Version, "."));
http_parse_version([Major|[Minor]]) ->
    http_parse_version({erlang:list_to_integer(Minor), erlang:list_to_integer(Major)});
http_parse_version({Major, Minor}) ->
    case {erlang:is_integer(Major), erlang:is_integer(Minor)} of
	{true, true} ->
	    {Minor, Major};
	_ ->
	    http_parse_version(error)
    end;
http_parse_version(_) ->
    {error, "HTTP version is garbage"}.

% list() -> {ok, [{atom(), list()}]}
http_parse_headers(Headers) ->
    http_parse_headers(string:tokens(Headers, "\r\n"), []).
http_parse_headers([Header|Headers], Result) ->
    case string:tokens(Header, ":") of
	[Field|[]] ->
	    error;
	[Field|Rest] ->
	    http_parse_headers(Headers, Result ++
				   [{erlang:list_to_atom(Field),
				     string:strip(string:join(Rest, ":"))}])
    end;
http_parse_headers([], Result) ->
    {ok, Result}.

handle_path(URI, HeadOnly, Headers) ->
    % FIXME: Need error handling here
    case file:read_file_info(URI) of
	{ok, Info} ->
	    case {Info#file_info.type, Info#file_info.access} of
		%% Send back 403 if we dont have access to the file
		{_, none} ->
		    http_error(403);
		{_, write} ->
		    http_error(403);
		{directory, _} ->
		    %% If there is a file called index.html, send back that.
		    case filelib:is_file(URI ++ "/index.html") of
			true ->
			    handle_path(URI ++ "/index.html", HeadOnly, Headers);
			false ->
			    {ok, Filelist} = file:list_dir(URI),
			    HTML = html_content("Index of " ++ URI, filelist(Filelist)),
			    ContentLen = erlang:integer_to_list(string:len(HTML)),

			    % Unfortunately we cant get the size of the file index without
			    % actually generating it. HEAD requests will therefore generate
			    % same server load as GET requests. The Content-Length header
			    % is not mandatory so sending it could be dropped. Apache does
			    % not send this header for directory listing
			    case HeadOnly of
				true ->
				    http_response(200, [{"Content-Length", ContentLen}], "");
				false ->
				    http_response(200, [{"Content-Length", ContentLen}], HTML)
			    end	
		    end;
		{regular, _} ->
		    % Much better, we can generate headers without reading
		    % the actual file.
		    case HeadOnly of
			true ->
			    Contents = "";
			false ->
			    {ok, Contents} = file:read_file(URI)
		    end,
		    ContentLen = erlang:integer_to_list(Info#file_info.size),
		    MimeLookup = whereis(mime_handler),
		    MimeLookup ! {self(), filename:extension(URI)},
		    receive
			MimeType ->
			    MimeType
		    end,	      
		    % FIXME: Timezones in Last-Modified is possibly handeled wrong.
		    http_response(200, [{"Content-Length", ContentLen},
					{"Last-Modified", httpd_util:rfc1123_date(Info#file_info.mtime)},
				       {"Content-Type", MimeType}], Contents)
	    end;
	{error, enoent} ->
	    http_error(404);
	{error, _} ->
	    http_error(500)
    end.

filelist([]) ->
    [];
filelist([File|Files]) ->
    ["<a href=\"" ++ File ++ "\">" ++ File ++ "</a><br>"] ++ filelist(Files).

% list(), [list()] -> list()
html_content(Title, Body) ->
    string:join(["<!DOCTYPE html>"
     "<html>",
     "<head>",
     "<title>" ++ Title ++ "</title>",
     "<meta encoding=\"utf-8\">",
     "</head>",
     "<body>"]
     ++ Body ++
	["</body>",
	 "</html>"], "\n").

http_error(Code) ->
    ErrLine = erlang:integer_to_list(Code)  ++ " " ++ httpd_util:reason_phrase(Code),
    ErrBody = html_content(ErrLine, ["<h1>" ++ ErrLine ++ "</h1>"]),
    http_response(Code, [], ErrBody).

http_response(Code, Headers, []) ->
    "HTTP/1.1 " ++ 
	erlang:integer_to_list(Code)
	++ " " ++
	httpd_util:reason_phrase(Code)
	++ "\r\n" ++
	header_format(default_headers(Headers, [])) ++ "\r\n";
http_response(Code, Headers, Body) ->
    http_response(Code,
		  Headers, []) ++ Body.

default_headers(Headers, []) ->
    Now = httpd_util:rfc1123_date(),
    default_headers(Headers, [{"Content-Type", "text/html; charset=UTF-8"},
			      {"Server", "Datanet webserver, Erlang "},
			      {"Connection", "close"},
			      {"Date", Now}]); 
default_headers([{Field, Value}|Headers], CurrentHeaders) ->
    case lists:keymember(Field, 1, Headers) of
	false ->
	    default_headers(Headers, lists:keystore(Field, 1, CurrentHeaders, {Field, Value}));
	true ->
	    default_headers(Headers, CurrentHeaders)
    end;
default_headers([], CurrentHeaders) ->
    CurrentHeaders.

header_format([{Key, Value}|Headers]) ->
    Key ++ ": " ++ Value ++ "\r\n" ++ header_format(Headers);
header_format([]) ->
    "".
    
