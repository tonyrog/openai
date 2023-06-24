%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%    Keep a log of map messages
%%% @end
%%% Created : 22 Jun 2023 by Tony Rogvall <tony@rogvall.se>

-module(openai_dialog).

-export([start/0, init/1]).
-export([open/2]).
-export([close/1]).
-export([stop/1]).
-export([reset/1]).
-export([add/3]).
-export([get/1]).

open(Pid, Filename) ->
    call(Pid, {open,Filename}).

close(Pid) ->
    call(Pid, close).

stop(Pid) ->
    call(Pid, stop).

reset(Pid) ->
    call(Pid, reset).

add(Pid, Role, Content) ->
    call(Pid, {add, Role, Content}).

get(Pid) ->
    call(Pid, get).

call(Pid, Call) ->
    call(Pid, Call, 5000).

call(Pid, Call, Timeout) ->
    Ref = make_ref(),
    Pid ! {call, [self()|Ref], Call},
    receive
	{reply, Ref, Answer} ->
	    Answer
    after Timeout ->
	    {error, timeout}
    end.

start() ->
    SELF = self(),
    Pid = spawn_link(fun() -> init(SELF) end),
    receive
	{Pid,started} ->
	    Pid
    end.

init(Parent) ->
    Parent ! {self(), started},
    loop(undefined, []).

loop(Fd, Messages) ->
    receive
	{call, From, {open, Filename}} ->
	    case open_(Filename) of
		{ok, {Fd1, []}} ->
		    if Fd =:= undefined ->
			    %% save any messages not saved so far
			    write_all_(Fd1, Messages),
			    reply_(From, ok),
			    loop(Fd1, Messages);
		       true ->
			    reply_(From, ok),
			    close_(Fd),
			    loop(Fd1, Messages)
		    end;
		{ok, {Fd1, Messages1}} ->
		    reply_(From, ok),
		    close_(Fd),
		    loop(Fd1, Messages1);
		Error ->
		    reply_(From, Error),
		    loop(Fd, Messages)
	    end;
	{call, From, close} ->
	    close_(Fd),
	    reply_(From, ok),
	    loop(undefined, []);
	{call, From, stop} ->
	    close_(Fd),
	    reply_(From, ok),
	    ok;
	{call, From, reset} ->
	    if Fd =:= undefined ->
		    reply_(From, ok),
		    loop(Fd, []);
	       true ->
		    file:position(Fd, bof),
		    file:truncate(Fd),
		    reply_(From, ok),
		    loop(Fd, [])
	    end;
	{call, From, {add,Role,Content}} ->
	    M = #{ role => fmt_role(Role),
		   content => fmt_content(Content), 
		   time => fmt_time(erlang:system_time(seconds)) },
	    write_(Fd, M),
	    Messages1 = Messages ++ [M],
	    reply_(From, ok),
	    loop(Fd, Messages1);
	{call, From, get} ->
	    reply_(From, lists:reverse(Messages)),
	    loop(Fd, Messages);

	{call, From, _Call} ->
	    reply_(From, {error, unknown_call}),
	    loop(Fd, Messages);
	
	_Command ->
	    io:format("opanai_dialog: got: ~p\n", [_Command]),
	    loop(Fd, Messages)
    end.


reply_([Pid|Ref], Reply) ->
    Pid ! {reply, Ref, Reply}.

open_(Filename) ->
    case file:open(Filename, [read, write]) of
	{ok, Fd} ->
	    case read_all_(Fd) of
		{ok, Ms} ->
		    {ok, {Fd, Ms}};
		Error ->
		    file:close(Fd),
		    Error
	    end;
	Error ->
	    Error
    end.

fmt_role(Rule) when is_atom(Rule) ->
    iolist_to_binary(atom_to_list(Rule)).

fmt_content(Content) when is_list(Content) ->
    unicode:characters_to_binary(Content);
fmt_content(Binary) when is_binary(Binary) ->
    Binary.




fmt_time(Time) ->
    iolist_to_binary(calendar:system_time_to_rfc3339(Time)).

close_(undefined) ->
    ok;
close_(Fd) ->
    file:close(Fd).

write_(undefined, _) ->
    ok;
write_(Fd, Term) ->
    file:write(Fd, io_lib:format("~p.\n", [Term])).

write_all_(undefined, _) ->
    ok;
write_all_(Fd, Messages) when is_list(Messages) ->
    lists:foreach(
      fun(M) ->
	      write_(Fd, M)
      end, Messages).

read_all_(Fd) ->
    read_all_(Fd, []).

read_all_(Fd, Ms) ->
    case io:read(Fd, '') of
	{ok, M} ->
	    read_all_(Fd, [M|Ms]);
	eof ->
	    {ok,Ms};
	Error ->
	    Error
    end.
