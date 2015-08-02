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
        erltsp_problem, [], [
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
        erltsp_solver_bb_single_simple, [], [
           erltsp_solver_bb_single_simple_iterate
        ]
    },{
        erltsp_api, [], [
            erltsp_api_test
        ]
    }
].

all() -> [
    {group, erltsp_problem},
    {group, erltsp_solver_sup},
    {group, erltsp_solver_evo_single},
    {group, erltsp_solver_bb_single_simple},
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
    Problem = get_problem(Config),
    {error, solution_incomplete} = erltsp_problem:solution(Problem, [1,2]),
    TrivialSolution = lists:seq(1,10),
    TrivialLength = 1291.454436262999,
    {ok, TrivialLength} = erltsp_problem:solution(Problem, TrivialSolution),
    TrivialSolutionRev = lists:reverse(TrivialSolution),
    {ok, TrivialLength} = erltsp_problem:solution(Problem, TrivialSolutionRev).

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
    {ok, _State1} = erltsp_solver_evo_single:iterate(State),
    ok.

erltsp_solver_evo_single_iterate_hard(Config) ->
    Solver = erltsp_solver_evo_single,
    File = data_dir(Config) ++ "n30_ts225.4.tspp",
    Problem = erltsp_problem:from_file(File),
    stress_solver(Solver, Problem, 15000).


% erltsp_solver_bb_single_simple
erltsp_solver_bb_single_simple_iterate(Config) ->
    Solver = erltsp_solver_bb_single_simple,
    File = data_dir(Config) ++ "n30_ts225.4.tspp",
    Problem = erltsp_problem:from_file(File),
    stress_solver(Solver, Problem, 15000).

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

get_problem(Config) ->
    File = data_dir(Config) ++ "n10_a280.1.tspp",
    erltsp_problem:from_file(File).

do_iterate(Solver, State, TRef) ->
    receive
        timeout -> {ok, State}
    after 0 ->
        handle_iterate(Solver:iterate(State), Solver, TRef)
    end.

handle_iterate({stop, State}, _, TRef) ->
    timer:cancel(TRef),
    ct:pal("stress solver finished early~n", []),
    {ok, State};
handle_iterate({ok, State}, Solver, TRef) ->
    do_iterate(Solver, State, TRef).    

stress_solver(Solver, Problem, TimeLimit) ->
    {ok, TRef} = timer:send_after(TimeLimit, timeout),
    {ok, State} = Solver:init(Problem),
    {ok, State1} = do_iterate(Solver, State, TRef),
    ct:pal("~p~n", [Solver:best(State1)]),
    ok.
