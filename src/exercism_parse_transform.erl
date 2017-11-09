-module(exercism_parse_transform).

%% API exports
-export([parse_transform/2]).

%%====================================================================
%% API functions
%%====================================================================

parse_transform(Form, _Option) ->
    sut(foo),
    io:format("~p~n", [Form]),
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
