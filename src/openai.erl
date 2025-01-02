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
-export([show_models/0]).
-export([get_model/1]).
-export([chat_compleations/1, chat_compleations/2, chat_compleations/3]).
-export([translate/1, translate/2]).
-export([transcribe/1, transcribe/2]).
-export([upload/1]).
-export([request_handler/4]).
-export([select_model/0, select_model/1]).
-export([default_model/0]).

-define(SERVER, ?MODULE).
-define(APP, ?MODULE).
-define(DEFAULT_FILTER, [{id, "^gpt.*"}, {time, {2023,6,26}}]).

-define(DEFAULT_BASEURL, "https://api.openai.com").
-define(DEFAULT_MODEL, "gpt-4").


%% -define(dbg(F, A), ok).
-define(dbg(F, A), io:format((F), (A))).

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
    WebPrompt = <<"Please pretend to be a web server only produce HTTP and HTML as output, first HTTP server replies with Content-Length header that is the length of the actual HTML content or other content. Be creative and interpret GET requests as prompts and generate dynamic web content accordingly.">>,
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
		    Response1 = adjust_http_response(Response),
		    ?dbg("[RESPONSE] ~p\n", [Response1]),
		    rester_socket:send(Socket, Response1),
		    %%rester_http_server:response_r(Socket, Request, 200, "OK", Response, []),
		    ok
	    end;
       true ->
	    rester_http_server:response_r(Socket, Request, 404, "Not Found",
					  "Object not found", [])
    end.

adjust_http_response(Response) ->
    case binary:match(Response, <<"Content-Length:">>) of
	{Pos,Len} ->
	    <<Before:Pos/binary, _:Len/binary, After/binary>> = Response,
	    [OldLen, After1] = binary:split(After, <<"\n">>),
	    [_, Content] = binary:split(After, <<"\n\n">>),
	    ContentLength = byte_size(Content),
	    io:format("Content-Length changed from ~p to ~w\n", 
		      [OldLen, ContentLength]),
	    iolist_to_binary([Before, 
			      "Content-Length: ", 
			      integer_to_list(ContentLength), "\n",
			      After1]);
	_ ->
	    Response
    end.
    
i() ->
    show_numbered_model_list().

default_model() ->
    list_to_binary(application:get_env(?APP, model, ?DEFAULT_MODEL)).

select_model() ->
    Models = numbered_model_list(),
    print_numbered_models(Models),
    NumberOfModels = length(Models),
    io:format("Select model: [1-~w]: ", [NumberOfModels]),
    Line = io:get_line(""),
    try list_to_integer(string:trim(Line)) of
	I when I > 0, I =< NumberOfModels ->
	    {I, ID, _} = lists:nth(I, Models),
	    Model = binary_to_list(ID),
	    application:set_env(?APP, model, Model),
	    Model;
	true ->
	    {error, bad_selection}
    catch
	error:badarg ->
	    {error, bad_selection}
    end.

select_model(Model) ->
    Model1 = iolist_to_binary([Model]),
    Models = numbered_model_list(),
    case lists:keyfind(Model1, 2, Models) of
	false ->
	    {error, {bad_model, Model}};
	{_I, _ID, _} ->
	    application:set_env(?APP, model, binary_to_list(Model1)),
	    ok
    end.


show_models() ->
    show_numbered_model_list(),
    ok.

numbered_model_list() ->
    numbered_model_list(?DEFAULT_FILTER).

numbered_model_list(Filter) ->
    {ok, #{ <<"data">> := Models }} = list_models(),
    filter_models(Models, 1, Filter).

filter_models([#{ <<"id">> := ID, <<"created">> := Time}|Models], I, Filter) ->
    case filter(Filter, ID, Time) of
	true ->
	    [{I, ID, Time} | filter_models(Models, I+1, Filter)];
	false ->
	    filter_models(Models, I, Filter)
    end;
filter_models([], _I, _Filter) ->
    [].


-define(UNIX_TIME, 62167219200).

filter([{id,Regex}|Rest], ID, Time) ->
    case re:run(ID, Regex) of
	{match, _} -> filter(Rest, ID, Time);
	nomatch -> false
    end;
filter([{time,Date={_Year,_Mon,_Day}}|Rest], ID, Time) ->
    DateTime = {Date,{0,0,0}},
    MatchTime = calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_TIME,
    if Time >= MatchTime -> 
	    filter(Rest, ID, Time);
       true -> false
    end;
filter([{time,DateTime={{_Year,_Mon,_Day},{_Hour,_Min,_Sec}}}|Rest],ID,Time) ->
    MatchTime = calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_TIME,
    if Time >= MatchTime -> 
	    filter(Rest, ID, Time);
       true -> false
    end;
filter([], _ID, _Time) ->
    true.
    

show_numbered_model_list() ->
    Models = numbered_model_list(),
    print_numbered_models(Models).

print_numbered_models(Models) ->
    Default = default_model(),
    DefaultNum = case lists:keyfind(Default, 2, Models) of
		     false -> 0;
		     {D, _, _} -> D
		 end,
    io:format("Current model: ~p ~w\n", [Default, DefaultNum]),
    lists:foreach(
      fun({I, ID, Created}) ->
	      Date = calendar:system_time_to_rfc3339(Created),
	      Def = if I =:= DefaultNum -> "*" ; true -> "" end,
	      io:format("~w~s: ~s [~s]\n", [I, Def, ID, Date])
      end, Models).    



list_models() ->
    Url = baseurl() ++ "/v1/models",
    wget(Url).


get_model(Model) when is_list(Model) ->
    Url = baseurl() ++ "/v1/models/"++Model,
    wget(Url).

				
chat_compleations(Messages) ->
    chat_compleations(model(), Messages, []).

chat_compleations(Model, Messages) ->
    chat_compleations(Model, Messages, []).

-type role() :: system | user | assistant | function.
-type message() :: #{ role => role(), content => binary(),
		      name => string(), function_call => term()}.
-type tool() :: term().

-spec chat_compleations(Model::string(), Messages::[message()], 
			Tools::[tool()]) ->
	  {ok, map()} | {error, Reason::term()}.

chat_compleations(Model, Messages, Tools) ->
    Url = baseurl() ++ "/v1/chat/completions",
    JBody0 = #{ <<"model">> => iolist_to_binary(Model), 
		  <<"messages">> => Messages },
    JBody = 
	case Tools of 
	    [] -> JBody0;
	    undefined -> JBody0;
	    [_|_] -> JBody0#{ <<"tools">> => Tools }
	end,
    wpost(Url, "application/json", JBody).


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

upload(Filename) ->
    Url = baseurl() ++ "/v1/files",
    wpost(Url, "multipart/form-data",
	  [{file,"file", "application/octet-stream",Filename}]).

%%
%% options:
%%   model => 'tts-1' | 'tts-1-hd'
%%   voice => alloy|echo|fable|onyx|nova|shimmer
%%   format => mp3 | opus | aac | flac
%%
speech(Text) ->
    speech(Text, latin1, #{}).

speech(Text, Charset, Options) ->
    Url = baseurl() ++ "/v1/audio/speech",
    Text1 = unicode:characters_to_binary(Text, Charset, utf8),
    Format = maps:get(format, Options, "mp3"),
    wpost_httpc(Url, "application/json", "audio/"++Format,
	  #{ <<"model">> => <<"tts-1">>,
	     <<"input">> => Text1,
	     <<"voice">> => maps:get(voice, Options, <<"alloy">>)
	   }).



wpost(Url, ContentType, Data) ->
    wpost(Url, ContentType, "application/json", Data).
wpost(Url, ContentType,  AcceptType, Data) ->
    {_User, ApiKey} = api_key("openai.com"),
    Hs = [{'Authorization', ["Bearer ", ApiKey]},
	  {'Content-Type', ContentType},
	  {'Accept',AcceptType}],
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

wpost_httpc(Url, ContentType, JsonData) ->
    wpost_httpc(Url, ContentType, "application/json", JsonData).
wpost_httpc(Url, ContentType, Accept, JsonData) ->
    {_User, ApiKey} = api_key("openai.com"),
    Hs = [{"Content-Type", ContentType},
	  {"Authorization", [<<"Bearer ">>, ApiKey]}],
    Body = jsone:encode(JsonData),
    ?dbg("POST: ~p\nHeaders: ~p\n", [Url, Hs]),
    ?dbg("Body:\n~s\n", [Body]),
    case httpc:request(post, {Url, Hs, Accept, Body},
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
   
