%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%    Text buffer
%%%
%%% Text position is handles as follows
%%%      Characters are 0 bases and mark the location before a charchter
%%%     position 0 is before the first character and position 1 is after
%%%     the first character and so on. -1 is after the last character.
%%%     -2 is before the last character.
%%%
%%%  Line positions are similar to character positions
%%%     The line before the first line is 0 and the line after the last 
%%%    line is -1
%%%
%%% FIXME: handle current position during various operations
%%% @end
%%% Created : 10 Nov 2023 by Tony Rogvall <tony@rogvall.se>

-module(openai_text_buffer).

-export([new/0, new/1]).
-export([new_file/1]).
-export([filename/1]).
-export([read_file/1, read_file/2]).
-export([insert_file/2]).
-export([write_file/2, write_file/3]).
-export([print/1, print/2, format/1, format/2]).
-export([insert/3, replace/3, move/3, delete/2]).
-export([find/2, find/3]).
-export([find_replace/3, find_replace/4]).
-export([refind/2, refind/3]).
-export([refind_replace/3, refind_replace/4]).
-export([insert_text_line/3, insert_text/3]).
%% clip
-export([setpos/2, getpos/1]).
-export([cut/2, copy/2, paste/2]).

%% utils
-export([get_text/1, get_text/2]).
-export([eof/0, bof/0]).

%% tests
-export([test/0]).
-export([test_insert_1a/0, test_insert_1b/0, test_insert_1c/0]).
-export([test_insert_2a/0, test_insert_2b/0, test_insert_2c/0]).
-export([test_delete_1a/0, test_delete_1b/0, test_delete_1c/0]).
-export([test_delete_2a/0, test_delete_2b/0, test_delete_2c/0]).
-export([test_delete_3a/0, test_delete_3b/0]).
-export([test_find_1a/0]).
-export([test_refind_1a/0]).
-export([test_find_replace_1a/0]).

%%-compile(export_all).

-define(Q, $\").

-define(UNIX_EOL,  $\n).
-define(WIN32_EOL, $\r,$\n).
-define(MAC_EOL,   $\r).
-define(DEFAULT_EOL, ?UNIX_EOL).

-define(is_re(RE), (element(1,(RE)) =:= re_pattern)).
-define(is_string(S), 
	(is_binary((S)) orelse ((S) =:= []) orelse is_integer(hd((S))))).
-define(is_text(T), (?is_string((T)) orelse ?is_string((hd((T)))))).

-type line_number() :: non_neg_integer().
-type character_position() :: non_neg_integer().
-type text_position() :: #{ line => line_number(), 
			    character => character_position()}.
-type text_range() :: #{ start => text_position(), 
			 stop => text_position()}.
-type codepoint() :: non_neg_integer().
-type color() :: {R::byte,G::byte,B::byte()} |
		 {A::byte(),R::byte(),G::byte(),B::byte()} |
		 ARGB::integer() | ColorName::(atom() | string()).
-record(character, 
	{
	 code :: codepoint(),
	 attr :: #{ color => color(), size => integer()}
	}).
-type character() :: codepoint() | #character{}.
-type text_line() :: [character()].
-type text() :: [text_line()].

-record(text_buffer,
	{
	 filename = "*scratch*":: file:filename(),
	 current :: undefined | text_position(),
	 clip   = [] :: text(),
	 buffer = [] :: text()
	}).
-type text_buffer() :: #text_buffer{}.

string_to_list(B) when is_binary(B) -> 
    unicode:characters_to_list(B);
string_to_list(L) when is_list(L) -> 
    L.

compile(RegExp) when ?is_string(RegExp) ->
    re:compile(RegExp);
compile(RE) when ?is_re(RE) ->
    {ok,RE}.



is_text([]) -> true;
is_text([Line|_]) when ?is_string(Line) -> true;
is_text(_) -> false.

-spec new() ->
	  text_buffer().
new() ->
    new([]).

-spec new(Text::string()) ->
	  text_buffer().

new(Text) when ?is_string(Text) ->
    #text_buffer { buffer = split_lines(string_to_list(Text)) }.

-spec new_file(FileName::string()) ->
	  text_buffer().
new_file(FileName) when ?is_string(FileName) ->
    FileName1 = string_to_list(FileName),
    #text_buffer { filename = FileName1 }.

filename(#text_buffer { filename = FileName }) ->
    FileName.

split_lines([]) -> [];
split_lines(Text) ->
    case is_text(Text) of
	true ->
	    lists:append([ split_text_data(Line) || Line <- Text ]);
	false -> %% assume text_line
	    split_text_data(Text)
    end.

%% Split text data into text lines treat \r\n, \n and \r as line separators
split_text_data(Data) when ?is_string(Data) ->
    lists:reverse(split_text_data(string_to_list(Data), [], [])).

split_text_data([?WIN32_EOL|Data], L, A) ->
    split_text_data(Data, [], [lists:reverse(L)|A]);
split_text_data([?UNIX_EOL|Data], L, A) ->
    split_text_data(Data, [], [lists:reverse(L)|A]);
split_text_data([?MAC_EOL|Data], L, A) ->
    split_text_data(Data, [], [lists:reverse(L)|A]);
split_text_data([C|Data], L, A) ->
    split_text_data(Data, [C|L], A);
split_text_data([], [], A) -> A;
split_text_data([], L, A) -> 
    [lists:reverse(L)|A].

-spec read_file(Filename::file:name()) ->
	  {ok,text_buffer()} | {error, Reason::file:posix()}.

read_file(Filename) ->
    case file:open(Filename, [read,{encoding,unicode}]) of
	{ok, Fd} ->
	    try load_lines(Fd, []) of
		{ok,Lines} ->
		    {ok, #text_buffer { filename = Filename,
				     current = undefined,
				     buffer  = Lines }};
		Error={error,_} -> Error
	    after
		file:close(Fd)
	    end;
	Error={error,_} -> Error
    end.

-spec read_file(Text_Buffer::text_buffer(), Filename::file:name()) ->
	  {ok,text_buffer()} | {error, Reason::file:posix()}.
read_file(Text_Buffer, Filename) ->
    case file:open(Filename, [read,{encoding,unicode}]) of
	{ok, Fd} ->
	    try load_lines(Fd, []) of
		{ok,Lines} ->
		    {ok, Text_Buffer#text_buffer { filename = Filename,
						   current = undefined,
						   buffer  = Lines }};
		Error={error,_} -> Error
	    after
		file:close(Fd)
	    end;
	Error={error,_} -> Error
    end.

%% Fixme: should insert at current position
-spec insert_file(Text_Buffer::text_buffer(), Filename::file:name()) ->
	  {ok,text_buffer()} | {error, Reason::file:posix()}.
insert_file(Text_Buffer, Filename) ->
    case file:open(Filename, [read,{encoding,unicode}]) of
	{ok, Fd} ->
	    try load_lines(Fd, []) of
		{ok,Lines} ->
		    Lines1 = Text_Buffer#text_buffer.buffer ++ Lines,
		    {ok, Text_Buffer#text_buffer { current = undefined,
						   buffer  = Lines1 }};
		Error={error,_} -> Error
	    after
		file:close(Fd)
	    end;
	Error={error,_} -> Error
    end.


load_lines(Fd, Acc) ->
    case file:read_line(Fd) of
	eof ->
	    {ok,lists:reverse(Acc)};
	Error={error,_} ->
	    Error;
	{ok,Line} ->
	    case lists:reverse(Line) of
		[?WIN32_EOL|Lin] ->
		    load_lines(Fd, [lists:reverse(Lin)|Acc]);
		[?UNIX_EOL|Lin] ->
		    load_lines(Fd, [lists:reverse(Lin)|Acc]);
		[?MAC_EOL|Lin] ->
		    load_lines(Fd, [lists:reverse(Lin)|Acc]);
		_ ->
		    load_lines(Fd, [Line|Acc])
	    end
    end.

-spec write_file(Text_Buffer::text_buffer(), Filename::file:name()) ->
	  ok | {error, Reason::file:posix()}.

write_file(Text_Buffer, Filename) ->
    %% fixme: select DEFAULT_EOL base on system
    write_file(Text_Buffer, Filename, [?DEFAULT_EOL]).

write_file(Text_Buffer, Filename, Eol) ->
    case file:open(Filename, [write, {encoding, unicode}]) of
	{ok, Fd} ->
	    Fmt = "~0.10.0w~ts"++Eol,
	    try write_lines(Fd, 0, Text_Buffer#text_buffer.buffer, Fmt) of
		Res -> Res
	    catch
		error:Reason ->
		    {error, Reason}
	    after
		file:close(Fd)
	    end;
	Error -> Error
    end.

write_lines(_Fd, _I, [], _Fmt) ->
    ok;
write_lines(Fd, I, [Line|Lines], Fmt) ->
    case file:write(Fd, io_lib:format(Fmt, [I, Line])) of
	ok -> write_lines(Fd, I+1, Lines, Fmt);
	Error -> Error
    end.

-spec number_of_lines(Text_Buffer::text_buffer()) -> integer().
number_of_lines(Text_Buffer) ->
    length(Text_Buffer#text_buffer.buffer).

-spec format(Text_Buffer::text_buffer()) -> 
	  binary().
format(Text_Buffer) ->
    format(Text_Buffer, []).

-spec format(Text_Buffer::text_buffer(), 
	     Opts::[{Key::atom(), Value::term()}]) ->
	  binary().
format(Text_Buffer, Opts) ->
    Eol = proplists:get_value(eol, Opts, [?DEFAULT_EOL]),
    Format = case proplists:get_value(line_numbers, Opts, false) of
		 false ->
		     "~0.10.0w~ts"++Eol;
		 true ->
		     NumLines = number_of_lines(Text_Buffer),
		     W=length(integer_to_list(NumLines-1)),
		     "~"++integer_to_list(W)++".10.0w: ~ts"++Eol
	     end,
    format_(0, Text_Buffer#text_buffer.buffer, Format, []).

format_(_I, [], _Format, Acc) ->
    unicode:characters_to_binary(lists:reverse(Acc));
format_(I, [Line|Lines], Format, Acc) ->
    format_(I+1, Lines, Format, [io_lib:format(Format, [I, Line])|Acc]).


-spec print(Text_Buffer::text_buffer()) ->
	  ok.
print(Text_Buffer) ->
    print(Text_Buffer, []).

-spec print(Text_Buffer::text_buffer(), Opts::[{Key::atom(), Value::term()}]) ->
	  ok.
print(Text_Buffer, Opts) ->
    Eol = proplists:get_value(eol, Opts, [?DEFAULT_EOL]),
    Format = case proplists:get_value(line_numbers, Opts, false) of
		 false ->
		     "~0.10.0w~ts"++Eol;
		 true ->
		     NumLines = number_of_lines(Text_Buffer),
		     W=length(integer_to_list(NumLines-1)),
		     "~"++integer_to_list(W)++".10.0w: ~ts"++Eol
	     end,
    write_lines(user, 0, Text_Buffer#text_buffer.buffer, Format).

-spec bof() -> text_position().
bof() ->
    #{ line => 0, character => 0}.

-spec eof() -> text_position().
eof() ->
    #{ line => -1, character => -1}.
	    
-spec insert_text_line(Text_Buffer::text_buffer(), 
		       InsertPos :: text_position(), TextLine::text_line()) ->
	  text_buffer().
insert_text_line(Text_Buffer, InsertPos, LineToInsert) 
  when ?is_string(LineToInsert) ->
    LineToInsert1 = string_to_list(LineToInsert),
    {Line,Pos} = get_char_position(Text_Buffer, InsertPos),
    {Before, [TextLine | After]} = select_line(Line, Text_Buffer#text_buffer.buffer),
    {B, A} = select_char(Pos, TextLine),
    TextLine1 = B ++ LineToInsert1 ++ A,
    Text_Buffer#text_buffer { buffer = Before ++ [TextLine1|After]}.


-spec insert_text(InsertPos :: text_position(), Text::text(), Text_Buffer::text_buffer()) ->
	  text_buffer().
insert_text(Text_Buffer, InsertPos, [Line1|Text]) when is_list(Line1) ->
    {Line,Pos} = get_char_position(Text_Buffer, InsertPos),
    case select_line(Line, Text_Buffer#text_buffer.buffer) of
	{Before, [TextLine | After]} ->
	    {B, A} = select_char(Pos, TextLine),
	    TextLine1 = (B ++ Line1),
	    Buffer = if A =:= [] ->
			     Before ++ [TextLine1 | Text] ++ After;
			true ->
			     Before ++ [TextLine1 | Text] ++ [A] ++ After
		     end,
	    Text_Buffer#text_buffer { buffer = Buffer };
	{Before, After} ->
	    Buffer = Before ++ [Line1|Text] ++ After,
	    Text_Buffer#text_buffer { buffer = Buffer }
    end;
insert_text(Text_Buffer, InsertPos, Text=[Line1|_]) when ?is_string(Line1) ->
    insert_text(InsertPos,
		[string_to_list(Line) || Line <- Text],
		Text_Buffer).

-spec insert(InsertPos :: text_position(), T::text()|text_line(),
	     Text_Buffer::text_buffer()) ->
	  text_buffer().
insert(Text_Buffer, InsertPos, Text=[TextLine|_]) when ?is_string(TextLine) ->
    insert_text(Text_Buffer, InsertPos, Text);
insert(Text_Buffer, InsertPos, Text) when ?is_string(Text) ->
    Text1 = string_to_list(Text),
    Lines = split_lines(Text1),
    case Lines of 
	[] ->
	    Text_Buffer;
	[Line1] ->
	    insert_text_line(Text_Buffer, InsertPos, Line1);
	_ ->
	    insert_text(Text_Buffer, InsertPos, Lines)
    end.

-spec paste(PastePos :: text_position(), Text_Buffer::text_buffer()) ->
	  text_buffer().
paste(Text_Buffer, PastePos) ->
    try get_char_position(Text_Buffer, PastePos) of
	{_Line, _Pos} ->
	    insert(Text_Buffer, PastePos, Text_Buffer#text_buffer.clip)
    catch
	error:_ ->
	    {Start, _Stop} = get_range(PastePos),
	    Text_Buffer1 = delete(Text_Buffer, PastePos),
	    insert(Text_Buffer1, Start, Text_Buffer#text_buffer.clip)
    end.

-spec delete(Range::text_range(), Text_Buffer::text_buffer()) ->
	  text_buffer().
delete(Text_Buffer, Range) ->
    {Start, Stop} = get_range(Range),
    {StartLine, StartPos} = get_char_position(Text_Buffer, Start),
    {StopLine, StopPos} = get_char_position(Text_Buffer, Stop),

    {Before, _After=[TextLine|After1]} = select_line(StartLine, Text_Buffer#text_buffer.buffer),
    {B, A} = select_char(StartPos, TextLine),
    if StartLine =:= StopLine ->
	    A1 = lists:nthtail(StopPos - StartPos, A),
	    TextLine1 = B ++ A1,
	    Text_Buffer#text_buffer { buffer = Before ++ [TextLine1 | After1] };
       StartLine < StopLine ->
	    N = StopLine - StartLine - 1,  %% strip N lines 1,1 - 3,1  => remove one line (line 2)
	    [NextTextLine | After2] = lists:nthtail(N, After1),
	    {_B2, A2} = select_char(StopPos, NextTextLine),
	    TextLine1 = B ++ A2,
	    Text_Buffer#text_buffer { buffer = Before ++ [TextLine1 | After2] }
    end.

-spec setpos(Text_Buffer::text_buffer(), Pos::text_position()) ->
	  text_buffer().
setpos(Text_Buffer, Pos) ->
    Text_Buffer#text_buffer { current = Pos }.

-spec getpos(Text_Buffer::text_buffer()) ->
	  text_position().
getpos(Text_Buffer) ->
    Text_Buffer#text_buffer.current.

-spec cut(Range::text_range(), Text_Buffer::text_buffer()) ->
	  text_buffer().
cut(Text_Buffer, Range) ->
    Cut = get_text(Text_Buffer, Range),
    Text_Buffer1 = delete(Text_Buffer, Range),
    Text_Buffer1#text_buffer { clip = Cut }.


-spec replace(Range::text_range(), NewText::text(), Text_Buffer::text_buffer()) ->
	  text_buffer().
replace(Text_Buffer, Range, NewText) when ?is_text(NewText) ->
    {Start,_End} = get_range(Range),
    Text_Buffer1 = delete(Text_Buffer, Range),
    insert(Text_Buffer1, Start, NewText).

-doc "Copy text from Range to Text_Buffer clip".
-spec copy(Range::text_range(), Text_Buffer::text_buffer()) ->
	  [text_buffer()].
copy( Text_Buffer, Range)  ->
    Copy = get_text(Text_Buffer, Range),
    Text_Buffer#text_buffer { clip = Copy }.


-spec move(Range::text_range(), Target::text_position(),Text_Buffer::text_buffer()) ->
	  text_buffer().
move(Text_Buffer, Range, Target) ->
    Text = get_text(Text_Buffer, Range),
    Text_Buffer1 = delete(Text_Buffer, Range),
    Text_Buffer2 = insert(Text_Buffer1, Target, Text),
    %% FIXME: howto remove non-overlapping area???
    Text_Buffer2.

%% Find Text starting from beginning of buffer
-spec refind(RegExp::text()|re:mp(), Text_Buffer::text_buffer()) ->
	  Range::text_range() | false.
refind(Text_Buffer, RE) when ?is_re(RE) ->
    refind_(RE, 0, Text_Buffer#text_buffer.buffer);
refind(Text_Buffer, RegExp) -> %% works for both list / binary
    {ok,RE} = compile(RegExp),
    refind_(RE, 0, Text_Buffer#text_buffer.buffer).

%% Find Text starting from position From 
-spec refind(ReText::text()|re:mp(), From::text_position(),
	     Text_Buffer::text_buffer()) ->
	  Range::text_range() | false.

refind(Text_Buffer, RE, From)  when ?is_re(RE) ->
    {Line, Pos} = get_char_position(Text_Buffer, From),
    {_Before, [TextLine | After]} = 
	select_line(Line, Text_Buffer#text_buffer.buffer),
    refind_(RE, Line, Pos, TextLine, After);
refind(Text_Buffer, RegExp, From) ->
    {ok,RE} = compile(RegExp),
    {Line, Pos} = get_char_position(Text_Buffer, From),
    {_Before, [TextLine | After]} = 
	select_line(Line, Text_Buffer#text_buffer.buffer),    
    refind_(RE, Line, Pos, TextLine, After).

refind_(RE, Line, Pos, TextLine, After) ->
    {_, RestTextLine} = select_char(Pos, TextLine),
    case re:run(RestTextLine, RE) of
	{match, [{MatchPos,MatchLen}|Groups]} ->
	    Groups1 = [{Pos+Mp,Ml} || {Mp,Ml} <- Groups],
	    #{ start => #{ line => Line, character => Pos+MatchPos},
	       stop => #{ line => Line, character => Pos+MatchPos+MatchLen},
	       groups => Groups1 };
	nomatch ->
	    refind_(RE, Line+1, After)
    end.

refind_(RE, Line, [TextLine|After]) ->
    case re:run(TextLine, RE) of
	{match, [{MatchPos,MatchLen}|Groups]} ->
	    #{ start => #{ line => Line, character => MatchPos},
	       stop => #{ line => Line, character => MatchPos+MatchLen },
	       groups => Groups
	     };
	nomatch ->
	    refind_(RE, Line+1, After)
    end;
refind_(_RE, _Line, []) ->
    false.


%% Find Text starting from beginning of buffer
-spec find(Text::text(), Text_Buffer::text_buffer()) ->
	  Range::text_range() | false.
find(Text_Buffer, Text) when ?is_string(Text) ->
    find_(string_to_list(Text), 0, Text_Buffer#text_buffer.buffer).

%% Find Text starting from position From 
-spec find(Text::text(), From::text_position(), Text_Buffer::text_buffer()) ->
	  Range::text_range() | false.

find(Text_Buffer, Text, From) when ?is_string(Text) ->
    {Line, Pos} = get_char_position(Text_Buffer, From),
    {_Before, [TextLine | After]} =
	select_line(Line, Text_Buffer#text_buffer.buffer),    
    find_(string_to_list(Text), Line, Pos, TextLine, After).

find_(Text, Line, Pos, TextLine, After) ->
    {_, RestTextLine} = select_char(Pos, TextLine),
    case find_string(RestTextLine, Text) of
	nomatch ->
	    find_(Text, Line+1, After);
	MatchPos ->
	    MatchLen = length(Text),
	    #{ start => #{ line => Line, character => Pos+MatchPos},
	       stop => #{ line => Line, character => Pos+MatchPos+MatchLen } }
    end.

find_(Text, Line, [TextLine|After]) ->
    case find_string(TextLine, Text) of
	nomatch ->
	    find_(Text, Line+1, After);
	MatchPos ->
	    MatchLen = length(Text),
	    #{ start => #{ line => Line, character => MatchPos},
	       stop => #{ line => Line, character => MatchPos+MatchLen } }
    end;
find_(_Text, _Line, []) ->
    false.

find_string(Text, Search) ->
    find_string_(Text, Text, 0, 0, Search, Search).

find_string_([C|Text], Text0, Pos, Pos0, [C|Search], Search0) ->
    find_string_(Text, Text0, Pos+1, Pos0, Search, Search0);
find_string_(_Text, _Text0, _Pos, Pos0, [], _Search0) ->
    Pos0;
find_string_(_Text, [_|Text0], _Pos, Pos0, _Search, Search0) ->
    find_string_(Text0, Text0, Pos0+1, Pos0+1, Search0, Search0);
find_string_(_Text, [], _Pos, _Pos0, _Search, _Search0) ->
    nomatch.


-spec find_replace(Text::text(), Replace::text(), Text_Buffer::text_buffer()) ->
	  text_buffer().
find_replace(Text_Buffer, Text, Replace) ->
    find_replace(Text_Buffer, Text, Replace, bof()).

-spec find_replace(Text_Buffer::text_buffer(), Text::text(), Replace::text(),
		   From::text_position()) ->
	  text_buffer().

find_replace(Text_Buffer, Text, Replace, From) when ?is_string(Text) ->
    find_replace_(Text_Buffer, string_to_list(Text), Replace, From).

find_replace_(Text_Buffer, Text, Replace, From) ->
    case find(Text_Buffer, Text, From) of
	false ->
	    Text_Buffer;
	Range ->
	    Text_Buffer1 = replace(Text_Buffer, Range, Replace),
	    {Start, _End} = get_range(Range),
	    find_replace_(Text_Buffer1, Text, Replace, 
			  next_char_position(Text_Buffer1, Start))
    end.


-spec refind_replace(Text_Buffer::text_buffer(), 
		     RegExp::text()|re:mp(), Replace::text()) ->
	  text_buffer().

refind_replace(Text_Buffer, RegExp, Replace) when ?is_string(Replace) ->
    {ok,RE} = compile(RegExp),
    Replace1 = string_to_list(Replace),
    refind_replace_(Text_Buffer, RE, Replace1, bof()).

-spec refind_replace(RegExp::text()|re:mp(), Replace::text(),
		     From::text_position(), Text_Buffer::text_buffer()) ->
	  text_buffer().

refind_replace(Text_Buffer, RegExp, Replace, From) when ?is_string(Replace) ->
    {ok,RE} = compile(RegExp),
    Replace1 = string_to_list(Replace),
    refind_replace_(Text_Buffer, RE, Replace1, From).

refind_replace_(Text_Buffer, RE, Replace, From) ->
    case refind(Text_Buffer, RE, From) of
	false ->
	    Text_Buffer;
	Range ->
	    Bound = bind(Text_Buffer, Range),
	    Replace1 = expand(Replace, Bound),
	    Text_Buffer1 = replace(Text_Buffer, Range, Replace1),
	    {Start, _End} = get_range(Range),
	    refind_replace_(Text_Buffer1, RE, Replace,
			    next_char_position(Text_Buffer1, Start))
    end.


bind(_Tb, #{ groups := [] }) -> 
    [];
bind(Tb, Range = #{ groups := Groups } ) ->
    bind_($1, Groups, Range, Tb).

bind_(I, [{Pos,Len}|Groups], Range = #{ start := Start }, Tb) ->
    #{ line := Line, character := _Pos1 } = Start,
    Range1 = #{ start => #{ line=>Line, character => Pos},
		stop => #{ line=>Line, character => Pos+Len}},
    Text = get_text(Tb, Range1),
    [{I,Text} | bind_(I+1, Groups, Range, Tb)];
bind_(_I, [], _Range, _Tb) ->
    [].


expand([$\\,C|Cs], Bound) when C >= $1, C =< $9 ->
    Value = proplists:get_value(C, Bound, ""),
    Value ++ expand(Cs, Bound);
expand([C|Cs], Bound) ->
    [C | expand(Cs, Bound)];
expand([], _) ->
    [].

-spec get_text(Text_Buffer::text_buffer()) ->
	  text().
get_text(Text_Buffer) ->
    get_text(Text_Buffer, #{ start => #{ line => 0, character => 0 },
			     stop  => #{ line => -1, character => -1 }}).

-spec get_text(Text_Buffer::text_buffer(), Range::text_range()) ->
	  text().
get_text(Text_Buffer, Range)  ->
    {Start, End} = get_range(Range),
    {StartLine, StartPos} = get_char_position(Text_Buffer, Start),
    {EndLine, EndPos} = get_char_position(Text_Buffer, End),

    case select_line(StartLine,Text_Buffer#text_buffer.buffer) of
	{[], []} ->
	    [];
	{_Before,[TextLine|After]} ->
	    {_B,A} = select_char(StartPos, TextLine),
	    if StartLine =:= EndLine ->
		    {Copy, _} = select_char(EndPos-StartPos, A),
		    Copy;
	       StartLine < EndLine ->
		    N = EndLine - StartLine - 1,  %% strip N lines 1,1 - 3,1  => remove one line (line 2)
		    {Ls,[NextLine|_]} = select_line(N, After),   %% Ls are the complete lines
		    {B1,_A1} = select_char(EndPos, NextLine), %% Get the part from end of range
		    [A]++Ls++[B1]
	    end
    end.

get_range(Range) ->
    case Range of
	#{ <<"start">> := Start, <<"stop">> := Stop} -> {Start, Stop};
	#{ start := Start, stop := Stop} -> {Start, Stop}
    end.

get_char_position(Text_Buffer, Position) ->
    case Position of
	#{ <<"line">> := Line, <<"character">> := Pos} ->
	    adjust_char_position(Text_Buffer, Line, Pos);
	#{ line := Line, character := Pos} ->
	    adjust_char_position(Text_Buffer, Line, Pos);
	#{ <<"line">> := Line} ->
	    adjust_char_position(Text_Buffer, Line, 0);
	#{ line := Line} ->
	    adjust_char_position(Text_Buffer, Line, 0)
    end.

adjust_char_position(Text_Buffer, Line, Pos) when is_integer(Line),
						  is_integer(Pos) ->
    Buf = Text_Buffer#text_buffer.buffer,
    Line1 = if Buf =:= [] -> 0;
	       Line < 0 -> length(Buf) + Line; 
	       true -> Line 
	    end,
    Pos1 = if Buf =:= [] -> 0;
	      Pos < 0 ->
		   TextLine = lists:nth(Line1+1, Buf),
		   length(TextLine) + Pos + 1;
	      true -> 
		   Pos 
	   end,
    {Line1, Pos1}.

next_char_position(Text_Buffer, Current) ->
    {Line, Pos} = get_char_position(Text_Buffer, Current),
    LineLength = length(lists:nth(Line+1, Text_Buffer#text_buffer.buffer)),
    Pos1 = Pos + 1,
    if Pos1 < LineLength ->
	    #{ line => Line, character => Pos1 };
       true ->
	    %% NumLines = length(Text_Buffer#text_buffer.buffer),
	    #{ line => Line + 1, character => 0 }
    end.

%% select line 
%% return {Before, After }
%%
-spec select_line(N::line_number(), Buffer::text()) ->
	  {Before::text(), Line::text_line(), After::text()}.
select_line(N, Buffer) when is_integer(N), N >= 0 ->
    lists:split(N, Buffer);
select_line(N, Buffer) when is_integer(N), N < 0 ->
    NumLines = length(Buffer),
    select_line(NumLines + N, Buffer).

-spec select_char(N::character_position(), Text::text_line()) ->
	  {A::text_line(),B::text_line()}.
select_char(N, Text) when is_integer(N), N >= 0 ->
    lists:split(N, Text);
select_char(N, Text) when is_integer(N), N < 0 ->
    NumChars = length(Text),
    select_char(NumChars + N, Text).	  
    
%% Test inserts
-define(ASSERT(Expr),
	try Expr of
	    true -> ok; 
	    false ->
		io:format("Assertion ~s FAILED\n", [??Expr]),
		exit(fail)
	catch 
	    error:_ ->
		io:format("Assertion ~s FAILED\n", [??Expr]),
		exit(fail)
	end).

-define(ASSERT_EQ(Expr1, Expr2),
	try Expr1 =:= Expr2 of
	    true -> ok; 
	    false ->
		io:format("Assertion ~p =:= ~p FAILED\n", [(Expr1), (Expr2)]),
		exit(fail)
	catch 
	    error:_ ->
		io:format("Assertion ~p =:= ~p FAILED\n", [(Expr1), (Expr2)]),
		exit(fail)
	end).

test() ->
    test_insert_1a(), test_insert_1b(),  test_insert_1c(),
    test_insert_2a(), test_insert_2b(),  test_insert_2c(),
    test_delete_1a(), test_delete_1b(),  test_delete_1c(),
    test_delete_2a(), test_delete_2b(),  test_delete_2c(),
    test_delete_3a(), test_delete_3b(),
    test_find_1a(),  
    test_refind_1a(), 
    test_find_replace_1a().


%% Insert a string before the first character in the first line
test_insert_1a() ->
    Text_Buffer = new("ABCDE"),
    Text_Buffer1 = insert(Text_Buffer, #{line=>0,character=>0}, "XYZ"),
    Buffer = Text_Buffer1#text_buffer.buffer,
    ?ASSERT(Buffer =:= ["XYZABCDE"]).


%% Insert a character in the before character $C of the first line
test_insert_1b() ->
    Text_Buffer = new("ABCDE"),
    Text_Buffer1 = insert(Text_Buffer, #{line=>0,character=>2}, "XYZ"),
    Buffer = Text_Buffer1#text_buffer.buffer,
    ?ASSERT(Buffer =:= ["ABXYZCDE"]).

%% Insert a string last in the first line
test_insert_1c() ->
    Text_Buffer = new("ABCDE"),
    Text_Buffer1 = insert(Text_Buffer, #{line=>0,character=>-1}, "XYZ"),
    Buffer = Text_Buffer1#text_buffer.buffer,
    ?ASSERT(Buffer =:= ["ABCDEXYZ"]).

%% Insert a line before the first line
test_insert_2a() ->
    Text_Buffer = new("ABCDE"),
    Text_Buffer1 = insert(Text_Buffer, #{line=>0,character=>0}, ["XYZ"]),
    Buffer = Text_Buffer1#text_buffer.buffer,
    ?ASSERT_EQ(Buffer,["XYZ", "ABCDE"]).

%% Insert a line after the first line
test_insert_2b() ->
    Text_Buffer = new("ABCDE\nFOOBAR\n"),
    Text_Buffer1 = insert(Text_Buffer, #{line=>1,character=>0}, ["XYZ"]),
    Buffer = Text_Buffer1#text_buffer.buffer,
    ?ASSERT_EQ(Buffer, ["ABCDE", "XYZ", "FOOBAR"]).

%% Insert a line after the last line
test_insert_2c() ->
    Text_Buffer = new("ABCDE\nFOOBAR\n"),
    Text_Buffer1 = insert(Text_Buffer, #{line=>-1,character=>-1}, ["","XYZ"]),
    Buffer = Text_Buffer1#text_buffer.buffer,
    ?ASSERT_EQ(Buffer,["ABCDE", "FOOBAR", "XYZ"]).

%% Remove a character 2 from the first line
test_delete_1a() ->
    T = new("ABCDE"),
    T1 = delete(T, #{ start => #{line=>0,character=>2},
		      stop => #{line=>0,character=>3}}),
    Buffer = T1#text_buffer.buffer,
    ?ASSERT_EQ(Buffer, ["ABDE"]).

%% Remove first character from the first line
test_delete_1b() ->
    T = new("ABCDE"),
    T1 = delete(T, #{ start => #{line=>0,character=>0},
		      stop => #{line=>0,character=>1}}),
    Buffer = T1#text_buffer.buffer,
    ?ASSERT_EQ(Buffer, ["BCDE"]).

%% Remove last character from the first line
test_delete_1c() ->
    T = new("ABCDE"),
    T1 = delete(T, #{ start => #{line=>0,character=>-2},
		      stop => #{line=>0,character=>-1}}),
    Buffer = T1#text_buffer.buffer,
    ?ASSERT_EQ(Buffer, ["ABCD"]).

%% Remove first line
test_delete_2a() ->
    T = new("ABCDE\nFOOBAR\n1234\n"),
    T1 = delete(T, #{ start => #{line=>0,character=>0},
		   stop => #{line=>1,character=>0}}),
    Buffer = T1#text_buffer.buffer,
    ?ASSERT_EQ(Buffer, ["FOOBAR", "1234"]).

%% Remove middle line
test_delete_2b() ->
    T = new("ABCDE\nFOOBAR\n1234\n"),
    T1 = delete(T, #{ start => #{line=>1,character=>0},
		   stop => #{line=>2,character=>0}}),
    Buffer = T1#text_buffer.buffer,
    ?ASSERT_EQ(Buffer, ["ABCDE", "1234"]).

%% Remove last line
test_delete_2c() ->
    T = new("ABCDE\nFOOBAR\n1234\n"),
    T1 = delete(T, #{ start => #{line=>-2,character=>-1},
		      stop => #{line=>-1,character=>-1}}),
    ?ASSERT_EQ(T1#text_buffer.buffer, ["ABCDE", "FOOBAR"]).

%% Clear middle line
test_delete_3a() ->
    T = new("ABCDE\nFOOBAR\n1234\n"),
    T1 = delete(T, #{ start => #{line=>1,character=>0},
		      stop => #{line=>1,character=>-1}}),
    Buffer = T1#text_buffer.buffer,
    ?ASSERT_EQ(Buffer, ["ABCDE", "", "1234"]).

%% Clear middle line after FOO
test_delete_3b() ->
    T = new("ABCDE\nFOOBAR\n1234\n"),
    T1 = delete(T, #{ start => #{line=>1,character=>3},
		      stop => #{line=>1,character=>-1}}),
    ?ASSERT_EQ(T1#text_buffer.buffer, ["ABCDE", "FOO", "1234"]).

%% Locate BAR in second line
test_find_1a() ->
    T = new("ABCDE\nFOOBAR\n1234\n"),
    L = find(T, "BAR"),
    ?ASSERT_EQ(L, #{ start => #{ line => 1, character => 3},
		     stop => #{ line => 1, character => 6}}).

test_refind_1a() ->
    T = new("ABCDE\nFOOBAR\n1234\n"),
    L = refind(T, "[0-9]+"),
    ?ASSERT_EQ(L, #{ start => #{ line => 2, character => 0},
		     stop => #{ line => 2, character => 4},
		     groups => []
		   }).

%% Replace Foo with Bar second line
test_find_replace_1a() ->
    T = new("ABCDE\nFooBar\n1234\n"),
    T1 = find_replace(T, "Foo", "Bar"),
    ?ASSERT_EQ(T1#text_buffer.buffer, ["ABCDE", "BarBar", "1234"]).
