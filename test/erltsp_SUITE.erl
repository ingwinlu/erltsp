-module(erltsp_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

init_per_suite(Config) ->
    application:ensure_all_started(erltsp),
    Config.

end_per_suite(_Config) ->
    ok.

groups() -> [
    {problem_loading, [], [
        parse_n10_a280_1,
        parse_all_problems
    ]}
].

all() -> [
    {group, problem_loading}
].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

% PROBLEM_LOADING

parse_n10_a280_1(Config) ->
    File = data_dir(Config) ++ "n10_a280.1.tspp",
    Problem = tsp_problem:from_file(File),

    10 = tsp_problem:dimension(Problem),
    970.29 = tsp_problem:threshold(Problem),

    Nodes = tsp_problem:nodelist(Problem),

    Node1 = dict:fetch(1, Nodes),
    1 = maps:get(id, Node1),
    32 = maps:get(x, Node1),
    65 = maps:get(y, Node1),

    Precedences = tsp_problem:precedences(Problem),
    [{1,5},{9,1}] == Precedences.

parse_all_problems(Config) ->
    TestDir = data_dir(Config),
    TestFiles = filelib:wildcard(TestDir ++ "*.tspp"),
    AllProblems = lists:map(fun
        (File) ->
            tsp_problem:from_file(File)
        end,
        TestFiles
    ),
    15 = length(AllProblems).

data_dir(Config) ->
    {_, DataDir} = lists:keyfind(data_dir, 1, Config),
    DataDir.
