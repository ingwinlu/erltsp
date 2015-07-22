-module(erltsp).

-export([start/0, stop/0]).
-export([all_solvers/0]).
-export([load_problem/1]).
-export([solver_run/2, solver_stop/0]).

-spec start() -> {ok, [StartedApp :: atom()]}.
start() ->
    application:ensure_all_started(erltsp).

-spec stop() -> ok.
stop() ->
    application:stop(erltsp).

-spec all_solvers() -> [Runner :: atom()].
all_solvers() ->
    [tsp_solver_evo_single].

-spec load_problem(File :: file:name_all()) -> Problem :: tsp_problem:tsp_problem().
load_problem(File) -> tsp_problem:from_file(File).

-spec solver_run(Problem :: tsp_problem:tsp_problem(), Solver :: atom()) -> ok.
solver_run(Problem, Solver) -> tsp_runner:run(Problem, Solver).

-spec solver_stop() -> {ok, State :: term(), Iterations :: non_neg_integer()}.
solver_stop() -> tsp_runner:stop().
