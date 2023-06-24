%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%    OpenAI API Client
%%% @end
%%% Created : 19 Jun 2023 by Tony Rogvall <tony@rogvall.se>

-module(openai).

-include_lib("kernel/include/file.hrl").
-include_lib("rester/include/rester_http.hrl").

-export([list_models/0]).
-export([get_model/1]).
-export([chat_compleations/1, chat_compleations/2]).
-export([translate/1, translate/2]).
-export([transcribe/1, transcribe/2]).
-export([request_handler/4]).

-define(SERVER, ?MODULE).
-define(APP, ?MODULE).

-define(DEFAULT_BASEURL, "https://api.openai.com").
-define(DEFAULT_MODEL, "gpt-3.5-turbo").

-define(dbg(F, A), ok).
%%-define(dbg(F, A), io:format((F), (A))).

-compile(export_all).

start() ->
    inets:start(),
    ssl:start(),
    application:ensure_all_started(rester),
    application:load(?MODULE).


%% a new empty chat loop
chat_web() ->
    Messages = [],
    {ok, LogFd} = file:open("chat.log", [write]),
    io:setopts(standard_io, [{encoding, unicode}]),
    SELF = self(),
    RH = {request_handler, {?MODULE, request_handler, SELF}},
%%    IOServer = spawn(fun() -> io_loop(standard_io, 1, SELF) end),
    IOServer = undefined,
    {ok,WebServer} = rester_http_server:start(8080, [RH]),
    ?dbg("WebServer: ~p\n", [WebServer]),
    WebPrompt = <<"Please pretend to be a web server only produce HTTP and HTML as output">>,
    {Content, Messages1} = chat_input(LogFd, <<"system">>, WebPrompt, Messages),
    erlang:display_string(iolist_to_binary(["Init Reponse: ", Content])),
    chat_loop(LogFd, IOServer, WebServer, Messages1),
    rester_http_server:stop(WebServer).

chat() ->
    Messages = [],
    {ok, LogFd} = file:open("chat.log", [write]),
    io:setopts(standard_io, [{encoding, unicode}]),
    SELF = self(),
    IOServer = spawn(fun() -> io_loop(standard_io, 1, SELF) end),
    WebServer = undefined,
    chat_loop(LogFd, IOServer, WebServer, Messages).


chat_loop(LogFd, IOServer, WebServer, Messages) ->
    ?dbg("\n[CHAT] loop_enter: ~p\n", [self()]),
    receive
	{IOServer, eof} ->
	    ok;
	{IOServer, Input} ->
	    {Content, Messages1} = 
		chat_input(LogFd, <<"user">>, Input, Messages),
	    io:format("\n~ts\n", [Content]),
	    chat_loop(LogFd, IOServer, WebServer, Messages1);
	{WebSession, Request} ->
	    Input = iolist_to_binary(Request),
	    ?dbg("\n[CHAT] web input: ~p\n", [Input]),
	    {Content, Messages1} = 
		chat_input(LogFd, <<"user">>, Input, Messages),
	    WebSession ! {self(), Content},
	    chat_loop(LogFd, IOServer, WebServer, Messages1)
    end.

chat_input(LogFd, Role, Input, Messages) ->
    Prompt = #{ <<"role">> => Role, <<"content">> => Input },
    Messages1 = Messages ++ [Prompt],
    case chat_compleations(Messages1) of
	{ok,#{<<"choices">> := [FirstChoice|_] }} ->
	    #{ <<"message">> := Message } = FirstChoice,
	    #{ <<"content">> := Content, <<"role">> := Role1 } = Message,
	    Reply = #{ <<"role">> => Role1, <<"content">> => Content },
	    Messages2 = Messages1 ++ [Reply],
	    io:format(LogFd, "~p.\n", [Prompt]),
	    io:format(LogFd, "~p.\n", [Reply]),
	    {Content, Messages2};
	Other ->
	    io:format("**ERROR* chat compleations failed ~p\n", 
		      [Other]),
	    {"", Messages}
    end.

io_loop(Fd, I, ChatServer) ->
    case io:get_line(Fd, integer_to_list(I)++"|>") of
	eof ->
	    ChatServer ! {self(), eof};
	"bye\n" ->
	    ChatServer ! {self(), eof};
	"quit\n" -> 
	    ChatServer ! {self(), eof};
	Line ->
	    Line8 = unicode:characters_to_binary(Line),
	    Input = string:trim(Line8, both),
	    ChatServer ! {self(), Input},
	    io_loop(Fd, I+1, ChatServer)
    end.


request_handler(Socket, Request, _Body, ChatServer) -> %% fixme XArgs!
    ?dbg("[CHAT] http request: chat=~p ~p\n, ~p\n", 
	 [ChatServer,Request, _Body]),
    Url = Request#http_request.uri,
    if Request#http_request.method =:= 'GET' ->
	    ChatServer ! {self(), "GET " ++ Url#url.path ++ "\n"},
	    receive
		{ChatServer, Response} ->
		    rester_socket:send(Socket, Response),
		    %%rester_http_server:response_r(Socket, Request, 200, "OK", Response, []),
		    ok
	    end;
       true ->
	    rester_http_server:response_r(Socket, Request, 404, "Not Found",
					  "Object not found", [])
    end.
    

list_models() ->
    Url = baseurl() ++ "/v1/models",
    wget(Url).

get_model(Model) when is_list(Model) ->
    Url = baseurl() ++ "/v1/models/"++Model,
    wget(Url).

-spec chat_compleations(Model::string(), 
			Messages::[#{ role => system | user | assistant | function, content => binary(), name => string(), function_call => term()}]) ->
	  {ok, map()} | {error, Reason::term()}.
				
chat_compleations(Messages) ->
    chat_compleations(model(), Messages).

chat_compleations(Model, Messages) ->
    Url = baseurl() ++ "/v1/chat/completions",
    wpost(Url, "application/json", 
	  #{ <<"model">> => iolist_to_binary(Model), 
	     <<"messages">> => Messages}).

transcribe(AudioFilename) ->
    transcribe(AudioFilename, []).
transcribe(AudioFilename, Opts) ->
    Url = baseurl() ++ "/v1/audio/transcriptions",    
    wpost(Url, "multipart/form-data",
	  [{data, "model", "whisper-1"} |
	   [{data, Key, Value} || {Key,Value} <- Opts]] ++
	      [{file,"file", "application/octet-stream",AudioFilename}]).

translate(AudioFilename) ->
    translate(AudioFilename, []).
translate(AudioFilename, Opts) ->
    Url = baseurl() ++ "/v1/audio/translations",    
    wpost(Url, "multipart/form-data",
	  [{data, "model", "whisper-1"} |
	   [{data, Key, Value} || {Key,Value} <- Opts]] ++
	      [{file,"file", "application/octet-stream",AudioFilename}]).


wpost(Url, ContentType,  Data) ->
    {_User, ApiKey} = api_key("openai.com"),
    Hs = [{'Authorization', ["Bearer ", ApiKey]},
	  {'Content-Type', ContentType},
	  {'Accept',"application/json"}],
    ?dbg("Post: ~p\nHeaders: ~p\n", [Url, Hs]),
    ?dbg("Data:\n~p\n", [Data]),
    case rester_http:wpost(Url, Hs, Data) of 
	{ok,Resp,Reply} when Resp#http_response.status =:= 200 ->
	    Headers = Resp#http_response.headers,
	    if Headers#http_shdr.content_type =:= "application/json" ->
		    {ok, jsone:decode(Reply)};
	       true ->
		    {Headers#http_shdr.content_type, Reply}
	    end;
	{ok,Resp,_Reply} -> 
	    {error, {bad_status, Resp#http_response.status}};
	{error, Reason} ->
	    {error, Reason}
    end.
	
wget(Url) ->
    {_User, ApiKey} = api_key("openai.com"),
    Hs = [{'Authorization', ["Bearer ", ApiKey]},
	  {'Accept',"application/json"}],
    case rester_http:wget(Url, Hs) of
	{ok,Resp,Reply} when Resp#http_response.status =:= 200 ->
	    Headers = Resp#http_response.headers,
	    if Headers#http_shdr.content_type =:= "application/json" ->
		    {ok, jsone:decode(Reply)};
	       true ->
		    {Headers#http_shdr.content_type, Reply}
	    end;
	{ok,Resp,_Body} -> 
	    {error, {bad_status, Resp#http_response.status}};
	{error, Reason} ->
	    {error, Reason}
    end.

wpost_httpc(Url, JsonData) ->
    {_User, ApiKey} = api_key("openai.com"),
    Hs = [{"Content-Type", "application/json"},
	  {"Authorization", [<<"Bearer ">>, ApiKey]}],
    Body = jsone:encode(JsonData),
    ?dbg("POST: ~p\nHeaders: ~p\n", [Url, Hs]),
    ?dbg("Body:\n~s\n", [Body]),
    case httpc:request(post, {Url, Hs, "application/json", Body},
		       [], [{body_format, binary}]) of
	{ok,{{_Vsn,200,_Ok}, Headers, ReplyBody}} ->
	    case proplists:get_value("content-type", Headers, "") of
		"application/json" ->
		    {ok, jsone:decode(ReplyBody)};
		Value ->
		    {Value, ReplyBody}
	    end;
	{ok,{{_Vsn,Status,_Ok}, _Headers, _Body}} ->
	    {error, {bad_status, Status}};
	{error, Reason} ->
	    {error, Reason}
    end.


wget_httpc(Url) ->
    {_User, ApiKey} = api_key("openai.com"),
    Hs = [{"Authorization", [<<"Bearer ">>, ApiKey]}],
    ?dbg("GET: ~p\nHeaders: ~p\n", [Url, Hs]),
    case httpc:request(get, {Url, Hs}, [], [{body_format, binary}]) of
	{ok,{{_Vsn,200,_Ok}, Headers, Body}} ->
	    case proplists:get_value("content-type", Headers, "") of
		"application/json" ->
		    {ok, jsone:decode(Body)};
		Value ->
		    {Value, Body}
	    end;
	{ok,{{_Vsn,Status,_Ok}, _Headers, _Body}} ->
	    {error, {bad_status, Status}};
	{error, Reason} ->
	    {error, Reason}
    end.

    
		
baseurl() ->
    case application:get_env(?APP, baseurl) of    
	undefined ->
	    ?DEFAULT_BASEURL;
	{ok,BaseUrl} ->
	    BaseUrl
    end.

model() ->
    case application:get_env(?APP, model) of    
	undefined ->
	    ?DEFAULT_MODEL;
	{ok,Model} ->
	    Model
    end.

%%
api_key(Host) ->
    case application:get_env(?APP, apikey) of
	undefined ->
	    get_auth_info(Host);
	{ok,".authinfo"} ->
	    get_auth_info(Host);
	{ok,"$OPENAI_API_KEY"} ->
	    {error, no_key_in_config};
	{ok, Key} ->
	    case application:get_env(?APP, user) of
		{ok,User} ->
		    {User,Key};
		_ ->
		    {"", Key}
	    end
    end.

%% machine HOST login NAME password VALUE port NUMBER
%% fixme: support gpg formatted file!
get_auth_info(Host) ->
    AuthInfoFilename = filename:join(os:getenv("HOME"),".authinfo"),
    get_auth_info(Host, AuthInfoFilename).

get_auth_info(Host, Filename) ->
    case file:open(Filename, [binary, read]) of
	{ok, Fd} ->
	    try get_auth_info_(Fd, iolist_to_binary(Host)) of
		Result ->
		    Result
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

get_auth_info_(Fd, Host) ->
    case file:read_line(Fd) of
	{ok, Line} ->
	    case binary:split(string:trim(Line), <<" ">>, [global,trim_all]) of
		[<<"machine">>,Host,<<"login">>,User,<<"password">>,Key|_] ->
		    {User,Key};
		[<<"machine">>,Host,<<"login">>,User,<<"apikey">>,Key|_] ->
		    {User,Key};
		_Line ->
		    %% io:format("NO-MATCH: ~p\n", [_Line]),
		    get_auth_info_(Fd, Host)
	    end;
	eof ->
	    false
    end.
		
	    
	    
    
    
