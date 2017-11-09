-module(exercism_parse_transform).

%% API exports
-export([parse_transform/2]).

%%====================================================================
%% API functions
%%====================================================================

parse_transform(Form, _Option) ->
    {ok, TestModuleName} = find_module_name(Form),
    io:format("ModuleName: ~p~n~n~p~n", [TestModuleName, Form]),
    Form.

%%====================================================================
%% Internal functions
%%====================================================================

sut(Module) ->
    {ok, Files} = file:list_dir("./src"),
    SUT = case lists:member("example.erl", Files) of
        true  -> example;
        false -> Module
    end,
    io:format("Subject under test: ~p~n", [SUT]),
    SUT.

find_module_name([]) -> {error, notfound};
find_module_name([{attribute,_Line,module,Module}|_]) -> {ok, Module};
find_module_name([_|T]) -> find_module_name(T).
