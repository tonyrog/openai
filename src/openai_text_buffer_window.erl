%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Text buffer window 
%%% @end
%%% Created : 20 Jan 2021 by Tony Rogvall <tony@rogvall.se>

-module(openai_text_buffer_window).
-behavious(epxw).

-include_lib("epx/include/epx_menu.hrl").

-export([start/0]).
-export([start/1]).

-export([read_file/2, insert_file/2, write_file/2]).
-export([setpos/2, getpos/1]).
-export([cut/2, copy/2, paste/1]).
-export([get_text/1, get_text/2]).

-export([insert/3, replace/3, move/3, delete/2]).
-export([find/2, find/3]).
-export([find_replace/3, find_replace/4]).
-export([refind/2, refind/3]).
-export([refind_replace/3, refind_replace/4]).

-export([init/1,
	 configure/2,
	 key_press/2,
	 key_release/2,
	 button_press/2,
	 button_release/2,
	 enter/2,
	 leave/2,
	 focus_in/2,
	 focus_out/2,
	 close/1,
	 draw/3,
	 command/3,
	 select/2,
	 motion/2
	 %% menu/2
	]).
-export([handle_info/2]).
-export([handle_call/3]).

%% -define(verbose(F,A), ok).
-define(verbose(F,A), io:format((F),(A))).

-define(WINDOW_WIDTH,  512).
-define(WINDOW_HEIGHT, 512).
-define(VIEW_WIDTH,    ?WINDOW_WIDTH).
-define(VIEW_HEIGHT,   ?WINDOW_HEIGHT).
-define(TEXT_COLOR, {0,0,0,0}).       %% black text
-define(LEFT_BORDER, 4).
-define(TOP_BORDER, 4).
-define(BORDER, 2).
-define(FONT_SIZE, 24).

-type text_position() :: openai_text_buffer:text_position().
-type text_range() :: openai_text_buffer:text_range().

-type text_line() :: openai_text_buffer:text_line().
-type text() :: openai_text_buffer:text().

%%
%% Client API
%%

-spec read_file(Pid::pid(), Filename::file:name()) ->
	  ok | {error, Reason::file:posix()}.
read_file(Pid, Filename) ->
    gen_server:call(Pid, {read_file, Filename}).

-spec insert_file(Pid::pid(), Filename::file:name()) ->
	  ok | {error, Reason::file:posix()}.
insert_file(Pid, Filename) ->
    gen_server:call(Pid, {insert_file, Filename}).

-spec write_file(Pid::pid(), Filename::file:name()) ->
	  ok | {error, Reason::file:posix()}.
write_file(Pid, Filename) ->
    gen_server:call(Pid, {write_file, Filename}).

-spec setpos(Pid::pid(), Pos::text_position()) -> 
	  text_position().
setpos(Pid, Pos) ->
    gen_server:call(Pid, {setpos, Pos}).

-spec getpos(Pid::pid()) -> text_position().
getpos(Pid) ->
    gen_server:call(Pid, getpos).

-spec cut(Pid::pid(), Range::text_range()) -> 
	  ok.
cut(Pid, Range) ->
    gen_server:call(Pid, {cut, Range}).

-spec copy(Pid::pid(), Range::text_range()) -> 
	  ok.
copy(Pid, Range) ->
    gen_server:call(Pid, {copy, Range}).

-spec paste(Pid::pid()) -> 
	  ok.
paste(Pid) ->
    gen_server:call(Pid, paste).

-spec get_text(Pid::pid()) -> text().
get_text(Pid) ->
	gen_server:call(Pid, get_text).

-spec get_text(Pid::pid(), Range::text_range()) -> text().
get_text(Pid, Range) ->
	gen_server:call(Pid, {get_text, Range}).

-spec insert(Pid::pid(), InsertPos::text_position(),T::text()|text_line()) ->
	  ok.
insert(Pid, InsertPos, T) ->
    gen_server:call(Pid, {insert, InsertPos, T}).

-spec replace(Pid::pid(), Range::text_range(), NewText::text()) ->
	  ok | {error, Reason::term()}.
replace(Pid, Range, NewText) ->
    gen_server:call(Pid, {replace, Range, NewText}).
-spec move(Pid::pid(), Range::text_range(), Target::text_position()) ->
	  ok | {error, Reason::term()}.
move(Pid, Range, Target) ->
	gen_server:call(Pid, {move, Range, Target}).
-spec delete(Pid::pid(), Range::text_range()) ->
	  ok.
delete(Pid, Range) ->
    gen_server:call(Pid, {delete, Range}).

-spec find(Pid::pid(),Text::text()) ->
	  Range::text_range() | false.
find(Pid, Text) ->
    gen_server:call(Pid, {find, Text}).

-spec find(Pid::pid(),Text::text(), From::text_position()) ->
	  Range::text_range() | false.
find(Pid, Text, From) ->
	gen_server:call(Pid, {find, Text, From}).

-spec find_replace(Pid::pid(),Text::text(), Replace::text()) ->
	  ok | {error, Reason::term()}.
find_replace(Pid, Text, Replace) ->
    gen_server:call(Pid, {find_replace, Text, Replace}).
-spec find_replace(Pid::pid(),Text::text(), Replace::text(), From::text_position()) ->
	  ok | {error, Reason::term()}.
find_replace(Pid, Text, Replace, From) ->
    gen_server:call(Pid, {find_replace, Text, Replace, From}).

-spec refind(Pid::pid(),RegExp::text()|re:mp()) ->
	  Range::text_range() | false.
refind(Pid, RegExp) ->
    gen_server:call(Pid, {refind, RegExp}).

-spec refind(Pid::pid(),ReText::text()|re:mp(), From::text_position()) ->
	  Range::text_range() | false.
refind(Pid, ReText, From) ->
    gen_server:call(Pid, {refind, ReText, From}).

-spec refind_replace(Pid::pid(),RegExp::text()|re:mp(), Replace::text()) ->
	  ok | {error, Reason::term()}.
refind_replace(Pid, RegExp, Replace) ->
    gen_server:call(Pid, {refind_replace, RegExp, Replace}).

-spec refind_replace(Pid::pid(),RegExp::text()|re:mp(), Replace::text(),
		     From::text_position()) ->
	  ok | {error, Reason::term()}.
refind_replace(Pid, RegExp, Replace, From) ->
    gen_server:call(Pid, {refind_replace, RegExp, Replace, From}).

start() ->
    start(openai_text_buffer:new()).

start(TB) ->
    application:ensure_all_started(epx),
    epxw:start(#{ module => ?MODULE },
	       [TB], 
	       [{title, openai_text_buffer:filename(TB)},
		{scroll_horizontal, bottom},  %% none|top|bottom
		{scroll_vertical,   left},    %% none|left|right
		{scroll_bar_color,  cyan},
		{scroll_hndl_color, blue},
		{screen_color,      white},
		{scroll_bar_size,   14},
		{scroll_hndl_size,  10},
		{left_bar,0},
		{top_bar,0},
		{right_bar,0},
		{width,?WINDOW_WIDTH},
		{height, ?WINDOW_HEIGHT},
		{view_width,?VIEW_WIDTH},
		{view_height,?VIEW_HEIGHT}]).

init([TB]) ->
    ?verbose("INIT: Opts=~w\n", [TB]),
    {ok,Font} = epx_font:match([{size,?FONT_SIZE}]),

    State = 
	#{ 
	   tb => TB,
	   font => Font,
	   selection => {0,0,0,0}
	 },
    {ok, State}.

configure(_Rect, State = #{ tb := TB} ) ->
    ?verbose("CONFIGURE: ~w\n", [_Rect]),
    {Sx,_Sy} = epxw:scale(),
    Scale = float(Sx*100),
    Status = io_lib:format("--- ~s L~w Dimension: ~wx~w, Scale: ~.2f%",
			   [openai_text_buffer:filename(TB), 1,
			    epxw:width(),epxw:height(),Scale]),
    epxw:set_status_text(Status),
    epxw:invalidate(),
    State.

key_press(_Event, State) ->
    ?verbose("KEY_PRESS: ~w\n", [_Event]),
    State.

key_release(_Event, State) ->
    ?verbose("KEY_RELEASE: ~w\n", [_Event]),
    State.

button_press(_Event, State) ->
    ?verbose("BUTTON_PRESS: ~w\n", [_Event]),
    State.

button_release(_Event, State) ->
    ?verbose("BUTTON_RELEASE: ~w\n", [_Event]),
    State.

enter(_Event, State) ->
    ?verbose("ENTER: ~w\n", [_Event]),
    State.

leave(_Event, State) ->
    ?verbose("LEAVE: ~w\n", [_Event]),
    State.

focus_in(_Event, State) ->
    ?verbose("FOCUS_IN: ~w\n", [_Event]),
    State.

focus_out(_Event, State) ->
    ?verbose("FOCUS_OUT: ~w\n", [_Event]),
    State.

close(State) ->
    ?verbose("CLOSE:\n", []),
    State.

%% draw/3 update content region (same as draw/4(content,...)
draw(Pixels, DirtyRect, State = #{ tb := TB, selection := Selection,
				   font := Font }) ->
    ?verbose("DRAW: Rect = ~p\n", [DirtyRect]),
    epx_gc:set_fill_style(solid),
    epx_gc:set_font(Font),
    %% {TxW,TxH}  = epx_font:dimension(Font,Text),
    epx_gc:set_foreground_color(?TEXT_COLOR),
    FH = epx:font_info(Font, pixel_size),
    X0 = ?LEFT_BORDER,
    Y0 = ?TOP_BORDER + epx:font_info(Font, ascent),

    {_DX, DY0, _DW, DH0} = DirtyRect,
    DY = trunc(DY0),
    DH = trunc(DH0),
    Y1 = DY - (DY rem FH),
    Y2 = (DY + DH + FH - 1) - ((DY + DH + FH - 1) rem FH),

    Lines = openai_text_buffer:get_text(TB),
    W0 = ?VIEW_WIDTH - 2*?LEFT_BORDER,
    {W1,N} = draw_lines(Lines, Pixels, X0, Y0, W0, FH, Y1, Y2, 0),
    io:format("Draw ~w lines\n", [N]),
    VH = length(Lines) * FH,
    H1 = VH, %% max(VH, ?VIEW_HEIGHT - 2*?TOP_BORDER),
    epxw:set_view_size(W1, H1),

    case Selection of
	undefined -> empty;
	{_,_,Sw,Sh} when Sw < 2, Sh < 2 -> empty;
	_ ->
	    epx_gc:set_fill_style(blend),
	    epx_gc:set_fill_color({127,127,127,127}),
	    epx_gc:set_border_color(black),
	    epx_gc:set_border_width(?BORDER),
	    epx:draw_rectangle(Pixels, Selection)
    end,
    State.

draw_lines([TextLine|Lines],Pixels,X,Y,W,FH,Y1,Y2,N) ->
    if Y < Y1 ->
	    draw_lines(Lines,Pixels,X,Y+FH,W,FH,Y1,Y2,N);
       Y >= Y2 ->
	    {W,N}; %%{X0, Y, W};
       true ->
	    Uft8TextLine = unicode:characters_to_binary(TextLine),
	    {X1,_Y1} = epx:draw_utf8(Pixels,X,Y,Uft8TextLine),
	    draw_lines(Lines,Pixels,X,Y+FH, max(W,X1),FH,Y1,Y2,N+1)
    end;
draw_lines([],_Pixels,_X0,_Y,W,_FH,_Y1,_Y2,N) ->
    {W,N}.

select({_Phase,Rect}, State) ->
    ?verbose("SELECT: ~w\n", [Rect]),
    State# { selection => Rect }.

motion(_Event={motion,_Button,_Pos}, State) ->
    ?verbose("MOTION: ~w\n", [_Event]),
    State.

command(Key, Mod, State) ->
    ?verbose("COMMAND: Key=~w, Mod=~w\n", [Key, Mod]),
    {reply, {Key,Mod}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% various
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(_Info, State) ->
    ?verbose("INFO: ~w\n", [_Info]),
    {noreply, State}.

handle_call({read_file, Filename}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: read_file: ~w\n", [Filename]),
    case openai_text_buffer:read_file(Tb, Filename) of
	{ok, Tb1} ->
	    epxw:invalidate(),
	    {reply, ok, State#{tb=>Tb1}};
	{error, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call({insert_file, Filename}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: insert_file: ~w\n", [Filename]),
    case openai_text_buffer:insert_file(Tb, Filename) of
	{ok, Tb1} ->
	    epxw:invalidate(),
	    {reply, ok, State#{tb=>Tb1}};
	{error, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call({write_file, Filename}, _From, State=#{ tb := Tb}) ->
    ?verbose("call: write_file: ~w\n", [Filename]),
    Result = openai_text_buffer:write_file(Tb, Filename),
    {reply, Result, State};
handle_call({setpos, Pos}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: setpos: ~p\n", [Pos]),
    Tb1 = openai_text_buffer:setpos(Tb, Pos),
    Pos1 = openai_text_buffer:getpos(Tb1),
    {reply, Pos1, State#{ tb => Tb1 }};
handle_call(getpos, _From, State=#{ tb := Tb }) ->
    ?verbose("call: getpos\n", []),
    Pos = openai_text_buffer:getpos(Tb),
    {reply, Pos, State};
handle_call({cut, Range}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: cut: ~p\n", [Range]),
    Tb1 = openai_text_buffer:cut(Tb, Range),
    epxw:invalidate(),
    {reply, ok, State#{ tb => Tb1 }};
handle_call({copy, Range}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: copy: ~p\n", [Range]),
    Tb1 = openai_text_buffer:copy(Tb, Range),
    {reply, ok, State#{ tb => Tb1 }};
handle_call(paste, _From, State=#{ tb := Tb }) ->
    ?verbose("call: paste:\n", []),
    Tb1 = openai_text_buffer:paste(Tb),
    epxw:invalidate(),
    {reply, ok, State#{ tb => Tb1 }};
handle_call({insert, InsertPos, T}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: insert: ~p, ~p\n", [InsertPos, T]),
    Tb1 = openai_text_buffer:insert(Tb, InsertPos, T),
    epxw:invalidate(),
    {reply, ok, State#{ tb => Tb1 }};
handle_call({replace, Range, NewText}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: replace: ~p ~p\n", [Range, NewText]),
    Tb1 = openai_text_buffer:replace(Tb, Range, NewText),
    epxw:invalidate(),
    {reply, ok, State#{ tb => Tb1 }};
handle_call({move, Range, Target}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: move: ~p ~p\n", [Range, Target]),
    Tb1 = openai_text_buffer:move(Tb, Range, Target),
    epxw:invalidate(),
    {reply, ok, State#{ tb => Tb1 }};
handle_call({delete, Range}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: delete: ~p\n", [Range]),
    Tb1 = openai_text_buffer:delete(Tb, Range),
    epxw:invalidate(),
    {reply, ok, State#{ tb => Tb1 }};
handle_call({find, Text}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: find: ~p\n", [Text]),
    Result = openai_text_buffer:find(Tb, Text),
    {reply, Result, State};
handle_call({find, Text, From}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: find: ~p ~p\n", [Text, From]),
    Result = openai_text_buffer:find(Tb, Text, From),
    {reply, Result, State};
handle_call({find_replace, Text, Replace}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: find_replace: ~p ~p\n", [Text, Replace]),
    Tb1 = openai_text_buffer:find_replace(Tb, Text, Replace),
    epxw:invalidate(),
    {reply, ok, State#{ tb => Tb1 }};
handle_call({find_replace, Text, Replace, From}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: find_replace: ~p ~p ~p\n", [Text, Replace, From]),
    Tb1 = openai_text_buffer:find_replace(Tb, Text, Replace, From),
    epxw:invalidate(),
    {reply, ok, State#{ tb => Tb1 }};

handle_call({refind, RegExp}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: refind: ~p\n", [RegExp]),
    Result = openai_text_buffer:refind(Tb, RegExp),
    {reply, Result, State};
handle_call({refind, RegExp, From}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: refind: ~p ~p\n", [RegExp, From]),
    Result = openai_text_buffer:refind(Tb, RegExp, From),
    {reply, Result, State};
handle_call({refind_replace, RegExp, Replace}, _From, State=#{ tb := Tb }) ->
    ?verbose("call: find_replace: ~p ~p\n", [RegExp, Replace]),
    Tb1 = openai_text_buffer:refind_replace(Tb, RegExp, Replace),
    epxw:invalidate(),
    {reply, ok, State#{ tb => Tb1 }};

handle_call({refind_replace, RegExp, Replace, Start}, _From, 
	    State=#{ tb := Tb }) ->
    ?verbose("call: find_replace: ~p ~p ~p\n", [RegExp, Replace, Start]),
    Tb1 = openai_text_buffer:refind_replace(Tb, RegExp, Replace, Start),
    epxw:invalidate(),
    {reply, ok, State#{ tb => Tb1 }};

handle_call(_Request, _From, State) ->
    ?verbose("CALL: ~w\n", [_Request]),
    {noreply, State}.
