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
        erltsp_solver_sup, [], [
            erltsp_solver_sup_run_evo_single
        ]
    },{
        erltsp_solver_evo_single, [], [
            erltsp_solver_evo_single_init,
            erltsp_solver_evo_single_iterate,
            erltsp_solver_evo_single_iterate_hard
        ]
    },{
        erltsp_api, [], [
            erltsp_api_test
        ]
    }
].

all() -> [
    {group, tsp_problem},
    {group, erltsp_solver_sup},
    {group, erltsp_solver_evo_single},
    {group, erltsp_api}
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

% ERLTSP Solver sup and Solver
erltsp_solver_sup_run_evo_single(Config) ->
    Problem = get_problem(Config),
    {ok, SolverPid} = erltsp_solver_sup:run(
                        Problem,
                        erltsp_solver_evo_single
    ),
    ok = timer:sleep(10000),
    ok = erltsp_solver_sup:stop(SolverPid),
    ok.

% TSP SOLVER EVO SINGLE
erltsp_solver_evo_single_init(Config) ->
    Problem = get_problem(Config),
    erltsp_solver_evo_single:init(Problem).

erltsp_solver_evo_single_iterate(Config) ->
    Problem = get_problem(Config),
    {ok, State} = erltsp_solver_evo_single:init(Problem),
    State1 = erltsp_solver_evo_single_iterate_(State, 1000),
    Threshold = tsp_problem:threshold(Problem),
    ct:pal("Threshold was ~p~n", [Threshold]),
    ok.

erltsp_solver_evo_single_iterate_hard(Config) ->
    File = data_dir(Config) ++ "n30_ts225.4.tspp",
    Problem = tsp_problem:from_file(File),
    {ok, State} = erltsp_solver_evo_single:init(Problem),

    State1 = erltsp_solver_evo_single_iterate_(State, 1000),
    State2 = erltsp_solver_evo_single_iterate_(State1, 1000),
    State3 = erltsp_solver_evo_single_iterate_(State2, 1000),
    State4 = erltsp_solver_evo_single_iterate_(State3, 2000),
    State5 = erltsp_solver_evo_single_iterate_(State4, 5000),
    State6 = erltsp_solver_evo_single_iterate_(State5, 5000),
    State7 = erltsp_solver_evo_single_iterate_(State6, 10000),
    ok.

erltsp_solver_evo_single_iterate_(State, 0) ->
    ct:pal("~p~n", [erltsp_solver_evo_single:best(State)]),
    State;
erltsp_solver_evo_single_iterate_(State, ToIterate) ->
    {ok, State1} = erltsp_solver_evo_single:iterate(State),
    erltsp_solver_evo_single_iterate_(State1, ToIterate-1).

%erltsp_bindings
erltsp_api_test(Config) ->
    Problem = get_problem(Config),
    {ok, Pid} = erltsp:solver_run(Problem, erltsp_solver_evo_single),
    timer:sleep(1000),
    {ok, Best} = erltsp:solver_best(Pid),
    ok = erltsp:solver_stop(Pid),
    ok.

% helpers
data_dir(Config) ->
    {_, DataDir} = lists:keyfind(data_dir, 1, Config),
    DataDir.

get_problem(Config) ->
    File = data_dir(Config) ++ "n10_a280.1.tspp",
    tsp_problem:from_file(File).
