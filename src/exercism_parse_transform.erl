%% @doc This module provides a parse transform to replace remote calls into the
%% module under test with calls into the example implementation if necessary.
%% 
%% To use this parse transform, add the following to your testing module:
%% 
%% ```
%% -include_lib("erl_exercism/include/exercism.hrl").
%% '''
%% 
%% Given a testing module `M_tests', all remote calls into `M' will be replaced
%% with calls into `example' if the module `example' is present in the project.
%% 
%% "Available" means, the file `src/example.erl' does exist.
%% @end

-module(exercism_parse_transform).

%% API exports
-export([parse_transform/2]).

%%====================================================================
%% API functions
%%====================================================================

parse_transform(Form, _Option) ->
    {ok, TestModuleName} = find_module_name(Form),
    {ok, ModuleName} = extract_tested_module(TestModuleName),

    case is_example() of
        false ->
            Form;
        true ->
            parse_trans:plain_transform(transform_fun(ModuleName), Form)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

is_example() ->
    {ok, Files} = file:list_dir("./src"),
    lists:member("example.erl", Files).

find_module_name([]) -> {error, notfound};
find_module_name([{attribute, _Line, module, Module} | _]) -> {ok, Module};
find_module_name([_ | T]) -> find_module_name(T).

extract_tested_module(ModuleName) when is_atom(ModuleName) ->
    ModuleName1 = atom_to_list(ModuleName),
    case re:run(ModuleName1, "^(.*)_tests$") of
        {match, [_, {0, Length}]} ->
            {ModuleName2, _} = lists:split(Length, ModuleName1),
            {ok, list_to_atom(ModuleName2)};
        _ ->
            error
    end.

transform_fun(Module) -> fun(F) -> do_transform(F, Module) end.

do_transform({call, LCall, {remote, LRemote, {atom, L1, ModuleName}, Fn}, Args}, ModuleName) ->
    TransformedArgs = parse_trans:plain_transform(transform_fun(ModuleName), Args),
    {call, LCall, {remote, LRemote, {atom, L1, example}, Fn}, TransformedArgs};
do_transform(_Form, _ModuleName) ->
    continue.

%%====================================================================
%% Internal testing
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(_assertExtract(Exp, Module), ?_assertMatch(Exp, extract_tested_module(Module))).

to_test_name(Text, Exp, Act) ->
    IoList = io_lib:format("~s (~p; ~p)", [Text, Exp, Act]),
    erlang:iolist_to_binary(IoList).

to_form(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    {ok, Form} = erl_parse:parse_form(Tokens),
    case is_list(Form) of
        true -> Form;
        false -> [Form]
    end.

extract_tested_module_test_() ->
    [
        ?_assertExtract({ok, hello_world}, hello_world_tests),
        ?_assertExtract({ok, zipper}, zipper_tests),
        ?_assertExtract(error, hello_world),
        ?_assertExtract(error, zipper)
    ].

transform_test_() ->
    Lines =
        [
            {"f() -> foo:bar().", "f() -> example:bar().", foo},
            {"f() -> foo:bar(foo:bar()).", "f() -> example:bar(example:bar()).", foo},
            {"f() -> foo:bar(foo:bar()).", "f() -> foo:bar(foo:bar()).", bar},
            {"f() -> foo:bar(bar:bar()).", "f() -> foo:bar(example:bar()).", bar},
            {"f() -> bar:bar(foo:bar()).", "f() -> example:bar(foo:bar()).", bar}
        ],
    Forms = lists:map(fun({From, To, Remote}) -> {to_form(From), to_form(To), Remote} end, Lines),
    Transformed = lists:map(
        fun({From, To, Remote}) ->
            {parse_trans:plain_transform(transform_fun(Remote), From), To}
        end,
        Forms
    ),
    lists:map(fun({Act, Exp}) -> ?_assertMatch(Exp, Act) end, Transformed).

find_module_name_test_() ->
    Modules = [
        {"-module(foo).", foo, foo_tests, error},
        {"-module(foo_tests).", foo_tests, foo_tests, {ok, foo}}
    ],
    Forms = lists:map(
        fun({Source, ModuleName, TestModuleName, TestedModuleResult}) ->
            {to_form(Source), ModuleName, TestModuleName, TestedModuleResult}
        end,
        Modules
    ),
    ModuleNames = lists:map(
        fun({Forms, ModuleName, _, _}) -> {find_module_name(Forms), ModuleName} end, Forms
    ),
    TestedModuleNames = lists:map(
        fun({_, ModuleName, _, TestedModuleResult}) ->
            {extract_tested_module(ModuleName), TestedModuleResult}
        end,
        Forms
    ),
    ModuleNameTests = lists:map(
        fun({Act, Exp}) ->
            {to_test_name("module name", Exp, Act), ?_assertMatch({ok, Exp}, Act)}
        end,
        ModuleNames
    ),
    TestedModuleNameTests = lists:map(
        fun({Act, Exp}) ->
            {to_test_name("tested module name", Exp, Act), ?_assertMatch(Exp, Act)}
        end,
        TestedModuleNames
    ),
    ModuleNameTests ++ TestedModuleNameTests.

-endif.
