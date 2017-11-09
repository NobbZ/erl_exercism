-module(exercism_parse_transform).

%% API exports
-export([parse_transform/2]).

%%====================================================================
%% API functions
%%====================================================================

parse_transform(Form, _Option) ->
    {ok, TestModuleName} = find_module_name(Form),
    {ok, ModuleName} = extract_tested_module(TestModuleName),
    io:format("ModuleName: ~p~n~n~p~n", [TestModuleName, Form]),

    Form1 = case is_example() of
        false ->
            Form;
        true ->
            replace_foreign_calls(Form, ModuleName)
    end,

    io:format("~p~n", [Form1]),
    Form1.

%%====================================================================
%% Internal functions
%%====================================================================

is_example() ->
    {ok, Files} = file:list_dir("./src"),
    lists:member("exaple.erl", Files).

find_module_name([]) -> {error, notfound};
find_module_name([{attribute,_Line,module,Module}|_]) -> {ok, Module};
find_module_name([_|T]) -> find_module_name(T).

extract_tested_module(ModuleName) when is_atom(ModuleName) ->
    ModuleName1 = atom_to_list(ModuleName),
    case re:run(ModuleName1, "^(.*)_tests$") of
        {match, [_, {Start, Length}]} ->
            ModuleName2 = string:slice(ModuleName1, Start, Length),
            {ok, list_to_atom(ModuleName2)};
        _ ->
            error
    end.

replace_foreign_calls([{function,LINE,Name,Arity,Clauses}|T], ModuleName) ->
    [{function, LINE, Name, Arity, replace_foreign_calls_in_clauses(Clauses, ModuleName)}
    |replace_foreign_calls(T, ModuleName)].

replace_foreign_calls_in_clauses([], _) -> [];
replace_foreign_calls_in_clauses([C|CS], ModuleName) ->
    io:format("Clause: ~p~n", [C]),
    [C|replace_foreign_calls_in_clauses(CS, ModuleName)].

%%====================================================================
%% Internal testing
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(_assertExtract(Exp, Module), ?_assertMatch(Exp, extract_tested_module(Module))).

extract_tested_module_test_() ->
    [?_assertExtract({ok, hello_world}, hello_world_tests)
    ,?_assertExtract({ok, zipper},      zipper_tests)
    ,?_assertExtract(error,             hello_world)
    ,?_assertExtract(error,             zipper)
    ].

-endif.
