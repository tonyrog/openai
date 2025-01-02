%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%    Text editor tool
%%% @end
%%% Created : 10 Nov 2023 by Tony Rogvall <tony@rogvall.se>

-module(openai_text_edit).

-export([load/1, run/3]).
-export([tools/0]).

-export([test1/0]).
-export([test2/0]).

-export([test_edit1/0]).
-export([test_edit2/0]).

-define(OBJECT, <<"object">>).
-define(STRING, <<"string">>).
-define(ARRAY, <<"array">>).
-define(INTEGER, <<"integer">>).

operations() ->
    [<<"load">>,
     <<"save">>,
     <<"insert">>, 
     <<"delete">>, 
     <<"replace">>,
     <<"copy">>,
     <<"move">> ].

command_type() ->
    #{
      '$schema' => <<"http://json-schema.org/draft-07/schema#">>,
      title => <<"Text API Schema">>,
      type => ?OBJECT,
      properties => 
	  #{
	    text => 
		#{
		  type => ?ARRAY,
		  description => <<"The text to be edited, represented as an array of strings, where each string makeup a line with out the end of line character.">>,
		  items => #{ type => ?STRING }
		 },
	    operations => 
		#{
		  type => ?ARRAY,
		  description => <<"List of text editing operations to be performed.">>,
		  items => 
		      #{
			type => ?OBJECT,
			properties => 
			    #{ operation => #{ type => ?STRING,
					       enum => operations(),
					       description => <<"Type of the operation.">>
					     },
			       position => #{ type => ?OBJECT,
					      description => <<"The target position for insert operations.">>,
					      properties => 
						  #{
						    line => 
							#{
							  type => ?INTEGER,
							  minimum => 0,
							  description => <<"Line number where the operation occurs.">>
							 },
						    character => 
							#{
							  type => ?INTEGER,
							  minimum => 0,
							  description => <<"Character position in the line.">>
							 }
						   },
					      required => [<<"line">>, <<"character">>]
					    },
			       range => 
				   #{
				     type => ?OBJECT,
				     description => <<"The range of text for delete, replace, or copy operations.">>,
				     properties => 
					 #{
					   'start' => #{
							'$ref' => <<"#/properties/operations/items/properties/position">>,
							description => <<"Start position of the range.">>
						       },
					   'stop' => #{
						       '$ref' => <<"#/properties/operations/items/properties/position">>,
						       description => <<"End position of the range.">>
						      }
					  },
				     required => [<<"start">>, <<"stop">>]
				    },
			       text => 
				   #{
				     type => ?STRING,
				     description => <<"Text to be inserted or used in replace operations.">>
				    },
			       source => 
				   #{
				     type => ?STRING,
				     description => <<"Source text to be replaced in text buffer if regular expression is not needed nor used.">>
				    },
			       regexp =>
				   #{
				     type => ?STRING,
				     description => <<"Regular expression used to match the text that is to be replaced.">>
				    },
			       filename => 
				   #{
				     type => ?STRING,
				     description => <<"Name of a file.">>
				    },
			       sourceRange => 
				   #{
				     '$ref' => <<"#/properties/operations/items/properties/range">>,
				     description => <<"Source range for copy or move operations.">>
				    },
			       targetPosition => 
				   #{
				     '$ref' => <<"#/properties/operations/items/properties/position">>,
				     description => <<"Target position for copy or move operations.">>
				    }
			     },
			required => [<<"operation">>]
		       }
		 }
	   },
      required => [<<"text">>, <<"operations">>]
     }.

tools() ->
    [
     #{type => <<"function">>,
       function =>
	   #{ name => <<"edit">>,
	      description => <<"Given an array of edit commands to manipulate edit buffer.">>,
	      parameters =>
		  #{ type => <<"object">>,
		     properties => 
			 #{
			   commands => command_type()
			  },
		     required => [<<"commands">>],
		     additionalProperties => false
		   }
	    }
      }
    ].

load(File) ->
    {ok, Tb} = openai_text_buffer:read_file(File),
    openai_text_buffer:get_text(Tb).

%% execute text operations
run(Tb, [Operation|Operations], St) ->
    case Operation of
	#{ <<"operation">> := <<"load">>,
	   <<"filename">> := FileName } ->
	    case openai_text_buffer:read_file(FileName) of
		{ok, Tb1} ->
		    run(Tb1, Operations, St);
		{error, enoent} ->
		    Tb2 = openai_text_buffer:new_file(FileName),
		    run(Tb2, Operations, St)
	    end;

	#{ <<"operation">> := <<"save">>,
	   <<"filename">> := FileName } ->
	    ok = openai_text_buffer:write_file(Tb, FileName),
	    run(Tb, Operations, St);

	#{ <<"operation">> := <<"insert">>,
	   <<"position">> := InsertPos,
	   <<"text">> := Text } ->
	    Tb1 = openai_text_buffer:insert(Tb, InsertPos, Text),
	    run(Tb1, Operations, St);

	#{<<"operation">> := <<"delete">>,
	  <<"range">> := Range } ->
	    Tb1 = openai_text_buffer:delete(Tb, Range),
	    run(Tb1, Operations, St);

	#{<<"operation">> := <<"replace">>,
	  <<"source">> := Source,
	  <<"text">> := NewText } ->
	    Tb1 = openai_text_buffer:find_replace(Tb,Source, NewText),
	    run(Tb1, Operations, St);

	#{<<"operation">> := <<"replace">>,
	  <<"regexp">> := RegExp,
	  <<"text">> := NewText } ->
	    Tb1 = openai_text_buffer:refind_replace(Tb,RegExp, NewText),
	    run(Tb1, Operations, St);

	#{<<"operation">> := <<"replace">>,
	  <<"range">> := Range,
	  <<"text">> := NewText } ->
	    Tb1 = openai_text_buffer:replace(Tb, Range, NewText),
	    run(Tb1, Operations, St);

	#{<<"operation">> := <<"copy">>,
	  <<"range">> := Range } ->
	    Tb1 = openai_text_buffer:copy(Tb,Range),
	    run(Tb1, Operations, St);

	#{<<"operation">> := <<"move">>,
	  <<"sourceRange">> := SourceRange,
	  <<"targetPosition">> := TargetPosition } ->
	    Tb1 = openai_text_buffer:move(Tb,SourceRange, TargetPosition),
	    run(Tb1, Operations, St);
	_ -> 
	    {error, {invalid_operation, Operation}}
    end;
run(Tb, [], St) ->
    {ok, Tb, St}.

%%
%% Test scripts
%%

%% test: replace data in a file
%%  1 - open file "data/source1.txt"
%%  2 - replace 16#<digits> with 0x<digits>
%%  3 - save file "data/destination1.txt"
%%
test1() ->
    St = #{},
    TB = openai_text_buffer:new(""),
    run(TB,
	[#{ <<"operation">> => <<"load">>,
	    <<"filename">> => <<"data/source1.txt">> },
	 #{ <<"operation">> => <<"replace">>,
	    <<"regexp">> => <<"16#([A-Fa-f0-9]+)">>,
	    <<"text">> => <<"0x\\1">> },
	 #{ <<"operation">> => <<"replace">>,
	    <<"regexp">> => <<"Erlang">>,
	    <<"text">> => <<"C">> },
	 #{ <<"operation">> => <<"replace">>,
	    <<"source">> => <<"constants">>,
	    <<"text">> => <<"const">> },

	 #{ <<"operation">> => <<"save">>,
	    <<"filename">> => <<"data/destination1.txt">> }
	 ],
	St).


%% rewrite C .h file with definitions into a .hrl file
test2() ->
    St = #{},
    TB = openai_text_buffer:new(""),
    run(TB,
	[#{ <<"operation">> => <<"load">>,
	    <<"filename">> => <<"data/source2.h">> },
	 #{ <<"operation">> => <<"replace">>,
	    <<"regexp">> => <<"0x([A-Fa-f0-9]+)">>,
	    <<"text">> => <<"16#\\1">> },

	 #{ <<"operation">> => <<"replace">>,
	    <<"source">> => <<"!">>, <<"text">> => <<" not ">> },
	 #{ <<"operation">> => <<"replace">>,
	    <<"source">> => <<"||">>, <<"text">> => <<" or ">> },
	 #{ <<"operation">> => <<"replace">>,
	    <<"source">> => <<"&&">>, <<"text">> => <<" and ">> },

	 #{ <<"operation">> => <<"replace">>,
	    <<"source">> => <<"~">>, <<"text">> => <<" bnot ">> },
	 #{ <<"operation">> => <<"replace">>,
	    <<"source">> => <<"|">>, <<"text">> => <<" bor ">> },
	 #{ <<"operation">> => <<"replace">>,
	    <<"source">> => <<"&">>, <<"text">> => <<" band ">> },

	 #{ <<"operation">> => <<"replace">>,
	    <<"regexp">> => <<"#define[ ]+([a-zA-Z_][a-zA-Z0-9_]*)(.*)">>,
	    <<"text">> => <<"-define(\\1,\\2).">> },

	 #{ <<"operation">> => <<"save">>,
	    <<"filename">> => <<"data/destination2.hrl">> }
	], St).

test_edit1() ->
    St0 = #{},
    Message = #{ role => <<"user">>,
		 content =>
		     <<"Create a file and insert a hello world C file into it, then save it as hello.c\n">>
	       },
    chat_tool_compleations(openai:default_model(),
			   [Message],
			   tools(), St0).


test_edit2() ->
    St0 = #{},
    Message = #{ role => <<"user">>,
		 content => 
		     <<"Load the file called hello.c and update the file to make an exit instead of return from the main program\n">>
	       },
    chat_tool_compleations(openai:default_model(),
			   [Message],
			   tools(), 
			   St0).

chat_tool_compleations(Model, Messages, Tools, St) ->
    case openai:chat_compleations(Model, Messages, Tools) of 
	{ok, Result} ->
	    io:format("Result: ~p~n", [Result]),
	    case tool_result(Model, Messages, Result, St) of
		{more,Messages1,St1} ->
		    chat_tool_compleations(Model, Messages1, [], St1);
		{stop,Messages1,St1} ->
		    {Messages1,St1}
	    end;
	{error, Reason} ->
	    io:format("Error: ~p~n", [Reason])
    end.

tool_result(Model, Messages, Result, St) ->
    case Result of
	#{ <<"choices">> := Choices } ->
	    tool_result_choices(Model, Messages, Choices, St);
	Other ->
	    {stop,Other,St}
    end.

tool_result_choices(Model, Messages, [Choice|Choices], St) ->
    case Choice of
	#{ <<"finish_reason">> := <<"tool_calls">> } ->
	    {Messages1,St1} = tool_calls(Messages, Choice, St),
	    tool_result_choices(Model, Messages1, Choices, St1);
	#{ <<"finish_reason">> := <<"stop">> } ->
	    {stop, Messages, St}
    end;
tool_result_choices(_, Messages, [], St) -> 
    {more, Messages, St}.

tool_calls(Messages, Choice, St) ->
    #{ <<"index">> := _Index, <<"message">> := Message } = Choice,
    #{ <<"tool_calls">> := Calls } = Message,
    function_calls(Messages++[Message], Calls, St).

function_calls(Messages, [Call|Calls], St) ->
    case Call of
	#{ <<"type">> := <<"function">>,
	   <<"function">> := Function,
	   <<"id">> := ID } ->
	    {Result,St1} = call_function(Function, ID, St),
	    function_calls(Messages ++ [Result], Calls, St1)
    end;
function_calls(Messages, [], St) ->
    {Messages, St}.

call_function(Function, ID, St) ->
    case Function of
	#{ <<"name">> := <<"edit">>,
	   <<"arguments">> := Args } ->
	    {Result,St1} = edit(Args, St),
	    {#{ <<"role">> => <<"tool">>,
		<<"content">> => Result,
		<<"tool_call_id">> => ID }, St1}
    end.

edit(Args,St) ->
    Json = json:decode(Args),
    jedit(Json,St).

jedit(JSon,St) ->
    io:format("jedit: ~p\n", [JSon]),
    Commands = maps:get(<<"commands">>, JSon, []),
    try run(openai_text_buffer:new(), Commands, St) of
	{ok,_Tb,St1} -> {<<"Ok">>, St1};
	TE -> {openai_text_buffer:format(TE), St}
    catch
	error:_ -> {<<"Error">>, St}
    end.
