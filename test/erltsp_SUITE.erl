-module(erltsp_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

init_per_suite(Config) ->
    application:ensure_all_started(erltsp),
    Config.

end_per_suite(_Config) ->
    ok.

groups() -> [
    {erltsp_problem, [], [
            parse_n10_a280_1,
            parse_all_problems,
            check_solutions
    ]},
    {erltsp_solver, [], [
            erltsp_solver_evo_single,
            erltsp_solver_bb_single_simple
    ]},
    {erltsp_api, [], [
            erltsp_api_test
    ]}
].

all() -> [
    {group, erltsp_problem},
    {group, erltsp_solver},
    {group, erltsp_api}
].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

% PROBLEM_LOADING

parse_n10_a280_1(Config) ->
    File = data_dir(Config) ++ "n10_a280.1.tspp",
    Problem = erltsp_problem:from_file(File),

    10 = erltsp_problem:dimension(Problem),
    970.29 = erltsp_problem:threshold(Problem),

    Nodes = erltsp_problem:nodedict(Problem),

    Node1 = dict:fetch(1, Nodes),
    1 = maps:get(id, Node1),
    32 = maps:get(x, Node1),
    65 = maps:get(y, Node1),

    Node2 = dict:fetch(2, Nodes),
    2 = maps:get(id, Node2),
    196 = maps:get(x, Node2),
    49 = maps:get(y, Node2),

    Edges = erltsp_problem:edgedict(Problem),
    Edge_1_2 = dict:fetch({1,2}, Edges),
    164.77863939236784 = maps:get(distance, Edge_1_2),

    Precedences = erltsp_problem:precedences(Problem),
    [{1,5},{9,1}] == Precedences.

parse_all_problems(Config) ->
    TestDir = data_dir(Config),
    TestFiles = filelib:wildcard(TestDir ++ "*.tspp"),
    AllProblems = lists:map(fun
        (File) ->
            erltsp_problem:from_file(File)
        end,
        TestFiles
    ),
    15 = length(AllProblems).

check_solutions(Config) ->
    File = data_dir(Config) ++ "n10_a280.1.tspp",
    Problem = erltsp_problem:from_file(File),
    {error, solution_incomplete} = erltsp_problem:solution(Problem, [1,2]),
    TrivialSolution = lists:seq(1,10),
    TrivialLength = 1291.454436262999,
    {ok, TrivialLength} = erltsp_problem:solution(Problem, TrivialSolution),
    TrivialSolutionRev = lists:reverse(TrivialSolution),
    {ok, TrivialLength} = erltsp_problem:solution(Problem, TrivialSolutionRev).

% ERLTSP Solver
erltsp_solver_evo_single(Config) ->
    Problem = get_hard_problem(Config),
    ok = run_solver(Problem, erltsp_solver_evo_single),
    ok.

erltsp_solver_bb_single_simple(Config) ->
    Problem = get_hard_problem(Config),
    ok = run_solver(Problem, erltsp_solver_bb_single_simple),
    ok.

run_solver(Problem, Solver) ->
    {ok, SolverPid} = erltsp_solver_sup:run(
                        Problem,
                        Solver
    ),
    ok = timer:sleep(15000),
    ok = erltsp_solver_sup:stop(SolverPid),
    ok.

%erltsp_bindings
erltsp_api_test(Config) ->
    FilePath = data_dir(Config) ++ "n10_a280.1.tspp",
    Problem = erltsp:load_problem(FilePath),
    {ok, Pid} = erltsp:solver_run(Problem, erltsp_solver_evo_single),
    timer:sleep(1000),
    {ok, Best} = erltsp:solver_best(Pid),
    ok = erltsp:solver_stop(Pid),
    ok.

% helpers
data_dir(Config) ->
    {_, DataDir} = lists:keyfind(data_dir, 1, Config),
    DataDir.

get_hard_problem(Config) ->
    File = data_dir(Config) ++ "n30_ts225.4.tspp",
    erltsp_problem:from_file(File).
