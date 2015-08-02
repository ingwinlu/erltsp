-module(erltsp).

-export([start/0, stop/0]).
-export([all_solvers/0]).
-export([load_problem/1]).
-export([solver_run/2, solver_stop/1, solver_best/1]).

-spec start() -> {ok, [StartedApp :: atom()]}.
start() ->
    application:ensure_all_started(erltsp).

-spec stop() -> ok.
stop() ->
    application:stop(erltsp).

-spec all_solvers() -> [Runner :: atom()].
all_solvers() ->
    [tsp_solver_evo_single, tsp_solver_bb_single_simple].

-spec load_problem(File :: file:name_all()) -> Problem :: erltsp_problem:problem().
load_problem(File) -> erltsp_problem:from_file(File).

-spec solver_run(Problem :: erltsp_problem:problem(),
                 Solver :: atom()) ->
    {ok, Pid :: pid()}.
solver_run(Problem, Solver) -> erltsp_solver_sup:run(Problem, Solver).

-spec solver_stop(Pid :: pid()) ->
    ok.
solver_stop(Pid) -> erltsp_solver_sup:stop(Pid).

-spec solver_best(Pid :: pid()) -> {ok, Best :: term()}.
solver_best(Pid) -> erltsp_solver:best(Pid).
