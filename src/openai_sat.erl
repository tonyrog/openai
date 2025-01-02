%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Simple SAT solver tool for openai api
%%% @end
%%% Created : 18 Dec 2024 by Tony Rogvall <tony@rogvall.se>

-module(openai_sat).

-export([formula_type/0, tools/0]).
-export([format/1, print/1]).
-export([test1/0, test2/0, test3/0]).
-export([format_model/1]).
-export([solve/1, jsolve/1]).
%% -compile(export_all).

%% tests
-export([test_solve/0, test_solve2/0]).
-export([test_solve3/0]).
-export([test_prove/0]).
-export([vincent_prove/0]).


%% Tools use
-define(FUN, <<"function">>).
-define(NAME, <<"name">>).
-define(PARAMETERS, <<"parameters">>).

%% FORMULA keys & enums
-define(OPERATOR_LIST, [<<"AND">>, <<"XOR">>, <<"OR">>, <<"NOT">>, 
			<<"IMPLIES">>, <<"EQUIVALENT">>]).


formula_type() ->
    #{
      type => <<"object">>,
      properties => 
	  #{
	    operator => 
		#{
		  type => <<"string">>,
		  enum => 
		      [<<"AND">>, <<"OR">>, <<"NOT">>, 
		       <<"IMPLIES">>, 
		       <<"EQUIVALENT">>]
		 },
	    operands => 
		#{
		  type => <<"array">>,
		  items => 
		      #{
			anyOf => 
			    [
			     #{ type => <<"string">> }, 
			     #{ '$ref' => <<"#">> }
			    ]
		       }
		 }
	   },
      required => [<<"operator">>, <<"operands">>]
     }.

tools() ->
    [
     #{type => <<"function">>,
       function =>
	   #{ name => <<"solve">>,
	      description => <<"Given a structured SAT formula solve and return False or a Model as a list of Var=Value.">>,
	      parameters =>
		  #{ type => <<"object">>,
		     properties => 
			 #{
			   formula => formula_type()
			  },
		     required => [<<"formula">>],
		     additionalProperties => false
		   }
	    }      
      },
     #{type => <<"function">>,
       function =>
	   #{ name => <<"prove">>,
	      description => <<"Proving a structured SAT formula, return True if foluma is valid / tautology or return a counter model that disprove the formula,">>,
	      parameters =>
		  #{ type => <<"object">>,
		     properties => 
			 #{
			   formula => formula_type()
			  },
		     required => [<<"formula">>],
		     additionalProperties => false
		   }
	    }      
      }
    ].


%% FORMULA
-define(formula, <<"formula">>).
-define(operator, <<"operator">>).
-define(operands, <<"operands">>).

format(#{ ?formula := Formula }) ->
    format_formula(Formula, "").

format_formula(#{ ?operator := Op, ?operands := [A] }, Parent) ->
    [L,R] = paren(Op, Parent),
    [L,Op, " ",format_formula(A,Op),R];
format_formula(#{ ?operator := Op, ?operands := A }, Parent) 
  when is_binary(A) ->
    [L,R] = paren(Op, Parent),
    [L,Op, " ",format_formula(A,Op),R];
format_formula(#{ ?operator := Op, ?operands := [A,B] }, Parent) ->
    [L,R] = paren(Op, Parent),
    [L, format_formula(A,Op)," ",Op," ",format_formula(B,Op),R];
format_formula(#{ ?operator := Op, ?operands := As }, Parent) 
  when is_list(As) ->
    [L,R] = paren(Op, Parent),
    Fs = [format_formula(A, Op) || A <- As],
    [L,lists:join([" ",Op," "], Fs),R];
format_formula(Var, _Parent) when is_binary(Var) ->
    [Var].

paren(Op, Parent) ->
    OpPrio = prio(Op),
    ParentPrio = prio(Parent),
    if OpPrio > ParentPrio ->
	    ["(", ")"];
       true ->
	    ["", ""]
    end.
    

prio(<<"NOT">>) -> 10;
prio(<<"AND">>) -> 20;
prio(<<"XOR">>) -> 30;
prio(<<"OR">>) -> 40;
prio(<<"IMPLIES">>) -> 50;
prio(<<"EQUIVALENT">>) -> 60;
prio(_) -> 70.
    
print(Formula) ->
    io:put_chars([format(Formula),"\n"]).

test1() ->
    #{ ?formula =>
	   #{ ?operator => "NOT",
	      ?operands => [ "A" ]
	    }
     }.

test2() ->
    #{ ?formula =>
	   #{ ?operator => "AND",
	      ?operands => 
		  [
		   #{ ?operator => "OR",
		      ?operands => [ "A", "B" ]
		    },
		   #{ ?operator => "IMPLIES",
		      ?operands => [ "C", "D" ]
		    }
		  ]
	      }
     }.

test3() ->
    #{ ?formula =>
	   #{ ?operator => "AND",
	      ?operands => 
		  [
		   #{ ?operator => "NOT",
		      ?operands => [ "A" ]
		    },
		   #{ ?operator => "NOT",
		      ?operands => 
			  [ #{ ?operator => "OR",
			       ?operands =>
				   ["C", "D"]} ]
		    }
		  ]
	    }
     }.

test_solve() ->
    Message = #{ role => <<"user">>,
		 content => 
		     <<"please prove that if Brian is Happy and if being Happy is being Rich then Brian is Rich">>
	       },
    openai:chat_compleations(openai:default_model(),
			     [Message],
			     tools()).


test_solve2() ->
    Message = #{ role => <<"user">>,
		 content =>
		     <<"We know that candles need oxygen to burn. But there's no oxygen in that room, so the candle must not be burning now.">>
	       },
    openai:chat_compleations(openai:default_model(),
			     [Message],
			     tools()).


test_solve3() ->
    Message = #{ role => <<"user">>,
		 content =>
		     <<"Find a solution for the following problem: (((A xor B) xor C) and (not A and not B and C)) and (((D xor E) xor F) and (not D and not E and F)) and (((G xor H) xor I) and (not G and not H and I)) and (((A xor D) xor G) and (not A and not D and G)) and (((B xor E) xor H) and (not B and not E and H)) and (((C xor F) xor I) and (not C and not F and I))">>
	       },
    chat_tool_compleations(openai:default_model(),
			   [Message],
			   tools()).

test_prove() ->
    Message = #{ role => <<"user">>,
		 content =>
		     <<"Prove that the propositional formula (P -> Q) == (~Q -> ~P)">>
	       },
    chat_tool_compleations(openai:default_model(),
			   [Message],
			   tools()).

vincent_prove() ->
    Message = #{ role => <<"user">>,
		 content =>
		     <<"Prove that the propositional formula (~q -> s) & (q -> p) & ((~q & s) -> r) & ~p -> r">>
	       },
    chat_tool_compleations(openai:default_model(),
			   [Message],
			   tools()).


chat_tool_compleations(Model, Messages, Tools) ->
    case openai:chat_compleations(Model, Messages, Tools) of 
	{ok, Result} ->
	    io:format("Result: ~p~n", [Result]),
	    case tool_result(Model, Messages, Result) of
		{more,Messages1} ->
		    chat_tool_compleations(Model, Messages1, []);
		{stop,Messages1} ->
		    Messages1
	    end;
	{error, Reason} ->
	    io:format("Error: ~p~n", [Reason])
    end.

tool_result(Model, Messages, Result) ->
    case Result of
	#{ <<"choices">> := Choices } ->
	    tool_result_choices(Model, Messages, Choices);
	Other ->
	    {stop,Other}
    end.

tool_result_choices(Model, Messages, [Choice|Choices]) ->
    case Choice of
	#{ <<"finish_reason">> := <<"tool_calls">> } ->
	    Messages1 = tool_calls(Messages, Choice),
	    tool_result_choices(Model, Messages1, Choices);
	#{ <<"finish_reason">> := <<"stop">> } ->
	    {stop, Messages}
    end;
tool_result_choices(_, Messages, []) -> 
    {more, Messages}.

tool_calls(Messages, Choice) ->
    #{ <<"index">> := _Index, <<"message">> := Message } = Choice,
    #{ <<"tool_calls">> := Calls } = Message,
    function_calls(Messages++[Message], Calls).

function_calls(Messages, [Call|Calls]) ->
    case Call of
	#{ <<"type">> := <<"function">>,
	   <<"function">> := Function,
	   <<"id">> := ID } ->
	    Result = call_function(Function, ID),
	    function_calls(Messages ++ [Result], Calls)
    end;
function_calls(Messages, []) ->
    Messages.

call_function(Function, ID) ->
    case Function of
	#{ <<"name">> := <<"solve">>,
	   <<"arguments">> := Args } ->
	    Result = solve(Args),
	    #{ <<"role">> => <<"tool">>,
	       <<"content">> => Result,
	       <<"tool_call_id">> => ID };
	#{ <<"name">> := <<"prove">>,
	   <<"arguments">> := Args } ->
	    Result = prove(Args),
	    #{ <<"role">> => <<"tool">>,
	       <<"content">> => Result,
	       <<"tool_call_id">> => ID }
    end.
	
solve(Args) ->
    Json = json:decode(Args),
    jsolve(Json).

jsolve(Json) ->
    io:format("Solve: formula = "),
    print(Json),
    Vp = varp_nif:new(#{xref => true}),
    X = build(Json, Vp),
    varp_nif:bind(Vp, X),
    varp_nif:push(Vp),
    case varp_circuit:bt(Vp) of
	true ->
	    M = varp_circuit:model(Vp, undefined),
	    iolist_to_binary(format_model(M));
	false ->
	    <<"False">>
    end.

prove(Args) ->
    Json = json:decode(Args),
    jprove(Json).

jprove(Json) ->
    io:format("Prove: formula = "),
    print(Json),
    Vp = varp_nif:new(#{xref => true}),
    X = build(Json, Vp),
    varp_nif:bind(Vp, varp_circuit:inv(X)),
    varp_nif:push(Vp),
    case varp_circuit:bt(Vp) of
	true ->
	    M = varp_circuit:model(Vp, undefined),
	    iolist_to_binary(format_model(M));
	false ->
	    <<"True">>
    end.

format_model([{{Var,[]},Value}]) ->
    case Value of
	true -> [[Var,"=True"]];
	false -> [[Var,"=False"]]
    end;
format_model([{{Var,[]},Value}|M]) ->
    case Value of
	true -> [[Var,"=True",","] | format_model(M)];
	false -> [[Var,"=False",","] | format_model(M)]
    end;
format_model([]) ->
    [].

%% build varp circuit from Json formula
build(#{ ?formula := Formula }, Vp) ->
    build_formula(Formula, Vp).

build_formula(#{ ?operator := Op, ?operands := [A] }, Vp) ->
    X = build_formula(A,Vp),
    case Op of
	<<"NOT">> -> varp_circuit:inv_gate(Vp, X)
    end;
build_formula(#{ ?operator := Op, ?operands := A }, Vp) when is_binary(A) ->
    X = build_formula(A,Vp),
    case Op of
	<<"NOT">> -> varp_circuit:inv_gate(Vp, X)
    end;
build_formula(#{ ?operator := Op, ?operands := [A,B] }, Vp) ->
    X = build_formula(A,Vp),
    Y = build_formula(B,Vp),
    case Op of
	<<"AND">> -> varp_circuit:and_gate(Vp, X, Y);
	<<"XOR">> -> varp_circuit:xor_gate(Vp, X, Y);
	<<"OR">> -> varp_circuit:or_gate(Vp, X, Y);
	<<"IMPLIES">> -> varp_circuit:imp_gate(Vp, X, Y);
	<<"EQUIVALENT">> -> varp_circuit:equ_gate(Vp, X, Y)
    end;
build_formula(#{ ?operator := Op, ?operands := As }, Vp) ->
    Bs = [build_formula(A, Vp) || A <- As],
    case Op of
	<<"AND">> -> varp_circuit:all(Vp, Bs);
	<<"OR">> -> varp_circuit:any(Vp, Bs);
	<<"XOR">> -> varp_circuit:odd(Vp, Bs);
	<<"EQUIVALENT">> -> varp_circuit:even(Vp, Bs)
    end;
build_formula(<<"NOT ",Var/binary>>, Vp) ->
    X = build_var(Var, Vp),
    varp_circuit:inv_gate(Vp, X);
build_formula(Var, Vp) when is_binary(Var) ->
    build_var(Var, Vp).

build_var(Var, Vp) ->
    case varp_nif:find_symbol(Vp, {Var,[]}) of
	false ->
	    varp_circuit:atom(Vp, Var);
	{bool,X} ->
	    X
    end.
