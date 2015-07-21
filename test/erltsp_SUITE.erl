-module(erltsp_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

init_per_suite(Config) ->
    application:ensure_all_started(erltsp),
    Config.

end_per_suite(_Config) ->
    ok.

groups() -> [
    {
        tsp_problem, [], [
            parse_n10_a280_1,
            parse_all_problems,
            check_solutions
        ]
    },{
        tsp_runner, [], [
            tsp_runner_states
        ]
    },{
        tsp_solver_evo_single, [], [
            tsp_solver_evo_single_init,
            tsp_solver_evo_single_iterate,
            tsp_solver_evo_single_iterate_hard
        ]
    }
].

all() -> [
    {group, tsp_problem},
    {group, tsp_runner},
    {group, tsp_solver_evo_single}
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

    Nodes = tsp_problem:nodedict(Problem),

    Node1 = dict:fetch(1, Nodes),
    1 = maps:get(id, Node1),
    32 = maps:get(x, Node1),
    65 = maps:get(y, Node1),

    Node2 = dict:fetch(2, Nodes),
    2 = maps:get(id, Node2),
    196 = maps:get(x, Node2),
    49 = maps:get(y, Node2),

    Edges = tsp_problem:edgedict(Problem),
    Edge_1_2 = dict:fetch({1,2}, Edges),
    164.77863939236784 = maps:get(distance, Edge_1_2),

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

check_solutions(Config) ->
    Problem = get_problem(Config),
    {error, solution_incomplete} = tsp_problem:solution(Problem, [1,2]),
    TrivialSolution = lists:seq(1,10),
    TrivialLength = 1291.454436262999,
    {ok, TrivialLength} = tsp_problem:solution(Problem, TrivialSolution),
    TrivialSolutionRev = lists:reverse(TrivialSolution),
    {ok, TrivialLength} = tsp_problem:solution(Problem, TrivialSolutionRev).

% TSP RUNNER
tsp_runner_states(Config) ->
    {error, undefined_problem} = tsp_runner:run(),
    Problem = get_problem(Config),
    ok = tsp_runner:set_problem(Problem),
    {error, undefined_timeout} = tsp_runner:run(),
    ok = tsp_runner:set_timeout(0),
    {error, negative_or_zero_timeout} = tsp_runner:run(),
    ok = tsp_runner:set_timeout(-1),
    {error, negative_or_zero_timeout} = tsp_runner:run(),
    ok = tsp_runner:set_timeout(60000),
    {error, undefined_solver} = tsp_runner:run(),
    ok = tsp_runner:set_solver(valid_solver),
    ok = tsp_runner:run(),
    ok = tsp_runner:stop().

% TSP SOLVER EVO SINGLE
tsp_solver_evo_single_init(Config) ->
    Problem = get_problem(Config),

    State = tsp_solver_evo_single:init(Problem),
    ct:pal("State: ~n~p~n", [State]),
    ok.

tsp_solver_evo_single_iterate(Config) ->
    Problem = get_problem(Config),
    State = tsp_solver_evo_single:init(Problem),
    State1 = tsp_solver_evo_single_iterate_(State, 1000),
    Threshold = tsp_problem:threshold(Problem),
    ct:pal("Threshold was ~p~n", [Threshold]),
    ok.

tsp_solver_evo_single_iterate_hard(Config) ->
    File = data_dir(Config) ++ "n30_ts225.4.tspp",
    Problem = tsp_problem:from_file(File),
    State = tsp_solver_evo_single:init(Problem),

    State1 = tsp_solver_evo_single_iterate_(State, 1000),
    Best1 = tsp_solver_evo_single:best(State1),
    ct:pal("~p:~p~n", [1000, Best1]),

    State2 = tsp_solver_evo_single_iterate_(State1, 1000),
    Best2 = tsp_solver_evo_single:best(State2),
    ct:pal("~p:~p~n", [2000, Best2]),

    State3 = tsp_solver_evo_single_iterate_(State2, 1000),
    Best3 = tsp_solver_evo_single:best(State3),
    ct:pal("~p:~p~n", [3000, Best3]),


    Threshold = tsp_problem:threshold(Problem),
    ct:pal("Threshold was ~p~n", [Threshold]),
    ok.


tsp_solver_evo_single_iterate_(State, ToIterate) ->
    tsp_solver_evo_single_iterate_(State, ToIterate, 0).

tsp_solver_evo_single_iterate_(State, 0, _) ->
    State;
tsp_solver_evo_single_iterate_(State, ToIterate, Iteration) ->
    State1 = tsp_solver_evo_single:evolve(State),
    tsp_solver_evo_single_iterate_(State1, ToIterate-1, Iteration+1).

% helpers
data_dir(Config) ->
    {_, DataDir} = lists:keyfind(data_dir, 1, Config),
    DataDir.

get_problem(Config) ->
    File = data_dir(Config) ++ "n10_a280.1.tspp",
    tsp_problem:from_file(File).
